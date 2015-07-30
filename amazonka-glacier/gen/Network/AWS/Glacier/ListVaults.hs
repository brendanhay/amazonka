{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.ListVaults
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- This operation lists all vaults owned by the calling user\'s account.
-- The list returned in the response is ASCII-sorted by vault name.
--
-- By default, this operation returns up to 1,000 items. If there are more
-- vaults to list, the response @marker@ field contains the vault Amazon
-- Resource Name (ARN) at which to continue the list with a new List Vaults
-- request; otherwise, the @marker@ field is @null@. To return a list of
-- vaults that begins at a specific vault, set the @marker@ request
-- parameter to the vault ARN you obtained from a previous List Vaults
-- request. You can also limit the number of vaults returned in the
-- response by specifying the @limit@ parameter in the request.
--
-- An AWS account has full permission to perform all operations (actions).
-- However, AWS Identity and Access Management (IAM) users don\'t have any
-- permissions by default. You must grant them explicit permission to
-- perform specific actions. For more information, see
-- <http://docs.aws.amazon.com/amazonglacier/latest/dev/using-iam-with-amazon-glacier.html Access Control Using AWS Identity and Access Management (IAM)>.
--
-- For conceptual information and underlying REST API, go to
-- <http://docs.aws.amazon.com/amazonglacier/latest/dev/retrieving-vault-info.html Retrieving Vault Metadata in Amazon Glacier>
-- and
-- <http://docs.aws.amazon.com/amazonglacier/latest/dev/api-vaults-get.html List Vaults>
-- in the /Amazon Glacier Developer Guide/.
--
-- <http://docs.aws.amazon.com/amazonglacier/latest/dev/api-ListVaults.html>
module Network.AWS.Glacier.ListVaults
    (
    -- * Request
      ListVaults
    -- ** Request constructor
    , listVaults
    -- ** Request lenses
    , lvMarker
    , lvLimit
    , lvAccountId

    -- * Response
    , ListVaultsResponse
    -- ** Response constructor
    , listVaultsResponse
    -- ** Response lenses
    , lvrsMarker
    , lvrsVaultList
    , lvrsStatus
    ) where

import           Network.AWS.Glacier.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Provides options to retrieve the vault list owned by the calling user\'s
-- account. The list provides metadata information for each vault.
--
-- /See:/ 'listVaults' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lvMarker'
--
-- * 'lvLimit'
--
-- * 'lvAccountId'
data ListVaults = ListVaults'
    { _lvMarker    :: !(Maybe Text)
    , _lvLimit     :: !(Maybe Text)
    , _lvAccountId :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListVaults' smart constructor.
listVaults :: Text -> ListVaults
listVaults pAccountId_ =
    ListVaults'
    { _lvMarker = Nothing
    , _lvLimit = Nothing
    , _lvAccountId = pAccountId_
    }

-- | A string used for pagination. The marker specifies the vault ARN after
-- which the listing of vaults should begin.
lvMarker :: Lens' ListVaults (Maybe Text)
lvMarker = lens _lvMarker (\ s a -> s{_lvMarker = a});

-- | The maximum number of items returned in the response. If you don\'t
-- specify a value, the List Vaults operation returns up to 1,000 items.
lvLimit :: Lens' ListVaults (Maybe Text)
lvLimit = lens _lvLimit (\ s a -> s{_lvLimit = a});

-- | The @AccountId@ value is the AWS account ID. This value must match the
-- AWS account ID associated with the credentials used to sign the request.
-- You can either specify an AWS account ID or optionally a single
-- apos@-@apos (hyphen), in which case Amazon Glacier uses the AWS account
-- ID associated with the credentials used to sign the request. If you
-- specify your account ID, do not include any hyphens (apos-apos) in the
-- ID.
lvAccountId :: Lens' ListVaults Text
lvAccountId = lens _lvAccountId (\ s a -> s{_lvAccountId = a});

instance AWSRequest ListVaults where
        type Sv ListVaults = Glacier
        type Rs ListVaults = ListVaultsResponse
        request = get
        response
          = receiveJSON
              (\ s h x ->
                 ListVaultsResponse' <$>
                   (x .?> "Marker") <*> (x .?> "VaultList" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance ToHeaders ListVaults where
        toHeaders = const mempty

instance ToPath ListVaults where
        toPath ListVaults'{..}
          = mconcat ["/", toBS _lvAccountId, "/vaults"]

instance ToQuery ListVaults where
        toQuery ListVaults'{..}
          = mconcat
              ["marker" =: _lvMarker, "limit" =: _lvLimit]

-- | Contains the Amazon Glacier response to your request.
--
-- /See:/ 'listVaultsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lvrsMarker'
--
-- * 'lvrsVaultList'
--
-- * 'lvrsStatus'
data ListVaultsResponse = ListVaultsResponse'
    { _lvrsMarker    :: !(Maybe Text)
    , _lvrsVaultList :: !(Maybe [DescribeVaultOutput])
    , _lvrsStatus    :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListVaultsResponse' smart constructor.
listVaultsResponse :: Int -> ListVaultsResponse
listVaultsResponse pStatus_ =
    ListVaultsResponse'
    { _lvrsMarker = Nothing
    , _lvrsVaultList = Nothing
    , _lvrsStatus = pStatus_
    }

-- | The vault ARN at which to continue pagination of the results. You use
-- the marker in another List Vaults request to obtain more vaults in the
-- list.
lvrsMarker :: Lens' ListVaultsResponse (Maybe Text)
lvrsMarker = lens _lvrsMarker (\ s a -> s{_lvrsMarker = a});

-- | List of vaults.
lvrsVaultList :: Lens' ListVaultsResponse [DescribeVaultOutput]
lvrsVaultList = lens _lvrsVaultList (\ s a -> s{_lvrsVaultList = a}) . _Default . _Coerce;

-- | FIXME: Undocumented member.
lvrsStatus :: Lens' ListVaultsResponse Int
lvrsStatus = lens _lvrsStatus (\ s a -> s{_lvrsStatus = a});
