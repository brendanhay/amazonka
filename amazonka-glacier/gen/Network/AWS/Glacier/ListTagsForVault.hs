{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.ListTagsForVault
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- This operation lists all the tags attached to a vault. The operation
-- returns an empty map if there are no tags. For more information about
-- tags, see
-- <http://docs.aws.amazon.com/amazonglacier/latest/dev/tagging.html Tagging Amazon Glacier Resources>.
--
-- <http://docs.aws.amazon.com/amazonglacier/latest/dev/api-ListTagsForVault.html>
module Network.AWS.Glacier.ListTagsForVault
    (
    -- * Request
      ListTagsForVault
    -- ** Request constructor
    , listTagsForVault
    -- ** Request lenses
    , ltfvrqAccountId
    , ltfvrqVaultName

    -- * Response
    , ListTagsForVaultResponse
    -- ** Response constructor
    , listTagsForVaultResponse
    -- ** Response lenses
    , ltfvrsTags
    , ltfvrsStatus
    ) where

import           Network.AWS.Glacier.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | The input value for @ListTagsForVaultInput@.
--
-- /See:/ 'listTagsForVault' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ltfvrqAccountId'
--
-- * 'ltfvrqVaultName'
data ListTagsForVault = ListTagsForVault'
    { _ltfvrqAccountId :: !Text
    , _ltfvrqVaultName :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListTagsForVault' smart constructor.
listTagsForVault :: Text -> Text -> ListTagsForVault
listTagsForVault pAccountId_ pVaultName_ =
    ListTagsForVault'
    { _ltfvrqAccountId = pAccountId_
    , _ltfvrqVaultName = pVaultName_
    }

-- | The @AccountId@ value is the AWS account ID of the account that owns the
-- vault. You can either specify an AWS account ID or optionally a single
-- apos@-@apos (hyphen), in which case Amazon Glacier uses the AWS account
-- ID associated with the credentials used to sign the request. If you use
-- an account ID, do not include any hyphens (apos-apos) in the ID.
ltfvrqAccountId :: Lens' ListTagsForVault Text
ltfvrqAccountId = lens _ltfvrqAccountId (\ s a -> s{_ltfvrqAccountId = a});

-- | The name of the vault.
ltfvrqVaultName :: Lens' ListTagsForVault Text
ltfvrqVaultName = lens _ltfvrqVaultName (\ s a -> s{_ltfvrqVaultName = a});

instance AWSRequest ListTagsForVault where
        type Sv ListTagsForVault = Glacier
        type Rs ListTagsForVault = ListTagsForVaultResponse
        request = get
        response
          = receiveJSON
              (\ s h x ->
                 ListTagsForVaultResponse' <$>
                   (x .?> "Tags" .!@ mempty) <*> (pure (fromEnum s)))

instance ToHeaders ListTagsForVault where
        toHeaders = const mempty

instance ToPath ListTagsForVault where
        toPath ListTagsForVault'{..}
          = mconcat
              ["/", toText _ltfvrqAccountId, "/vaults/",
               toText _ltfvrqVaultName, "/tags"]

instance ToQuery ListTagsForVault where
        toQuery = const mempty

-- | Contains the Amazon Glacier response to your request.
--
-- /See:/ 'listTagsForVaultResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ltfvrsTags'
--
-- * 'ltfvrsStatus'
data ListTagsForVaultResponse = ListTagsForVaultResponse'
    { _ltfvrsTags   :: !(Maybe (Map Text Text))
    , _ltfvrsStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListTagsForVaultResponse' smart constructor.
listTagsForVaultResponse :: Int -> ListTagsForVaultResponse
listTagsForVaultResponse pStatus_ =
    ListTagsForVaultResponse'
    { _ltfvrsTags = Nothing
    , _ltfvrsStatus = pStatus_
    }

-- | The tags attached to the vault. Each tag is composed of a key and a
-- value.
ltfvrsTags :: Lens' ListTagsForVaultResponse (HashMap Text Text)
ltfvrsTags = lens _ltfvrsTags (\ s a -> s{_ltfvrsTags = a}) . _Default . _Map;

-- | FIXME: Undocumented member.
ltfvrsStatus :: Lens' ListTagsForVaultResponse Int
ltfvrsStatus = lens _ltfvrsStatus (\ s a -> s{_ltfvrsStatus = a});
