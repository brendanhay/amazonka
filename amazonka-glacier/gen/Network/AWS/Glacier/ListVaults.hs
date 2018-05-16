{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.ListVaults
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation lists all vaults owned by the calling user's account. The list returned in the response is ASCII-sorted by vault name.
--
--
-- By default, this operation returns up to 1,000 items. If there are more vaults to list, the response @marker@ field contains the vault Amazon Resource Name (ARN) at which to continue the list with a new List Vaults request; otherwise, the @marker@ field is @null@ . To return a list of vaults that begins at a specific vault, set the @marker@ request parameter to the vault ARN you obtained from a previous List Vaults request. You can also limit the number of vaults returned in the response by specifying the @limit@ parameter in the request.
--
-- An AWS account has full permission to perform all operations (actions). However, AWS Identity and Access Management (IAM) users don't have any permissions by default. You must grant them explicit permission to perform specific actions. For more information, see <http://docs.aws.amazon.com/amazonglacier/latest/dev/using-iam-with-amazon-glacier.html Access Control Using AWS Identity and Access Management (IAM)> .
--
-- For conceptual information and underlying REST API, see <http://docs.aws.amazon.com/amazonglacier/latest/dev/retrieving-vault-info.html Retrieving Vault Metadata in Amazon Glacier> and <http://docs.aws.amazon.com/amazonglacier/latest/dev/api-vaults-get.html List Vaults > in the /Amazon Glacier Developer Guide/ .
--
--
-- This operation returns paginated results.
module Network.AWS.Glacier.ListVaults
    (
    -- * Creating a Request
      listVaults
    , ListVaults
    -- * Request Lenses
    , lvMarker
    , lvLimit
    , lvAccountId

    -- * Destructuring the Response
    , listVaultsResponse
    , ListVaultsResponse
    -- * Response Lenses
    , lvrsMarker
    , lvrsVaultList
    , lvrsResponseStatus
    ) where

import Network.AWS.Glacier.Types
import Network.AWS.Glacier.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Provides options to retrieve the vault list owned by the calling user's account. The list provides metadata information for each vault.
--
--
--
-- /See:/ 'listVaults' smart constructor.
data ListVaults = ListVaults'
  { _lvMarker    :: !(Maybe Text)
  , _lvLimit     :: !(Maybe Text)
  , _lvAccountId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListVaults' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lvMarker' - A string used for pagination. The marker specifies the vault ARN after which the listing of vaults should begin.
--
-- * 'lvLimit' - The maximum number of vaults to be returned. The default limit is 1000. The number of vaults returned might be fewer than the specified limit, but the number of returned vaults never exceeds the limit.
--
-- * 'lvAccountId' - The @AccountId@ value is the AWS account ID. This value must match the AWS account ID associated with the credentials used to sign the request. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon Glacier uses the AWS account ID associated with the credentials used to sign the request. If you specify your account ID, do not include any hyphens ('-') in the ID.
listVaults
    :: Text -- ^ 'lvAccountId'
    -> ListVaults
listVaults pAccountId_ =
  ListVaults'
    {_lvMarker = Nothing, _lvLimit = Nothing, _lvAccountId = pAccountId_}


-- | A string used for pagination. The marker specifies the vault ARN after which the listing of vaults should begin.
lvMarker :: Lens' ListVaults (Maybe Text)
lvMarker = lens _lvMarker (\ s a -> s{_lvMarker = a})

-- | The maximum number of vaults to be returned. The default limit is 1000. The number of vaults returned might be fewer than the specified limit, but the number of returned vaults never exceeds the limit.
lvLimit :: Lens' ListVaults (Maybe Text)
lvLimit = lens _lvLimit (\ s a -> s{_lvLimit = a})

-- | The @AccountId@ value is the AWS account ID. This value must match the AWS account ID associated with the credentials used to sign the request. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon Glacier uses the AWS account ID associated with the credentials used to sign the request. If you specify your account ID, do not include any hyphens ('-') in the ID.
lvAccountId :: Lens' ListVaults Text
lvAccountId = lens _lvAccountId (\ s a -> s{_lvAccountId = a})

instance AWSPager ListVaults where
        page rq rs
          | stop (rs ^. lvrsMarker) = Nothing
          | stop (rs ^. lvrsVaultList) = Nothing
          | otherwise =
            Just $ rq & lvMarker .~ rs ^. lvrsMarker

instance AWSRequest ListVaults where
        type Rs ListVaults = ListVaultsResponse
        request = get glacier
        response
          = receiveJSON
              (\ s h x ->
                 ListVaultsResponse' <$>
                   (x .?> "Marker") <*> (x .?> "VaultList" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable ListVaults where

instance NFData ListVaults where

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
--
--
-- /See:/ 'listVaultsResponse' smart constructor.
data ListVaultsResponse = ListVaultsResponse'
  { _lvrsMarker         :: !(Maybe Text)
  , _lvrsVaultList      :: !(Maybe [DescribeVaultOutput])
  , _lvrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListVaultsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lvrsMarker' - The vault ARN at which to continue pagination of the results. You use the marker in another List Vaults request to obtain more vaults in the list.
--
-- * 'lvrsVaultList' - List of vaults.
--
-- * 'lvrsResponseStatus' - -- | The response status code.
listVaultsResponse
    :: Int -- ^ 'lvrsResponseStatus'
    -> ListVaultsResponse
listVaultsResponse pResponseStatus_ =
  ListVaultsResponse'
    { _lvrsMarker = Nothing
    , _lvrsVaultList = Nothing
    , _lvrsResponseStatus = pResponseStatus_
    }


-- | The vault ARN at which to continue pagination of the results. You use the marker in another List Vaults request to obtain more vaults in the list.
lvrsMarker :: Lens' ListVaultsResponse (Maybe Text)
lvrsMarker = lens _lvrsMarker (\ s a -> s{_lvrsMarker = a})

-- | List of vaults.
lvrsVaultList :: Lens' ListVaultsResponse [DescribeVaultOutput]
lvrsVaultList = lens _lvrsVaultList (\ s a -> s{_lvrsVaultList = a}) . _Default . _Coerce

-- | -- | The response status code.
lvrsResponseStatus :: Lens' ListVaultsResponse Int
lvrsResponseStatus = lens _lvrsResponseStatus (\ s a -> s{_lvrsResponseStatus = a})

instance NFData ListVaultsResponse where
