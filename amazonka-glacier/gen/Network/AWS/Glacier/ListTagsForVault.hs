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
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation lists all the tags attached to a vault. The operation
-- returns an empty map if there are no tags. For more information about
-- tags, see
-- <http://docs.aws.amazon.com/amazonglacier/latest/dev/tagging.html Tagging Amazon Glacier Resources>.
--
-- /See:/ <http://docs.aws.amazon.com/amazonglacier/latest/dev/api-ListTagsForVault.html AWS API Reference> for ListTagsForVault.
module Network.AWS.Glacier.ListTagsForVault
    (
    -- * Creating a Request
      ListTagsForVault
    , listTagsForVault
    -- * Request Lenses
    , ltfvAccountId
    , ltfvVaultName

    -- * Destructuring the Response
    , ListTagsForVaultResponse
    , listTagsForVaultResponse
    -- * Response Lenses
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
-- * 'ltfvAccountId'
--
-- * 'ltfvVaultName'
data ListTagsForVault = ListTagsForVault'
    { _ltfvAccountId :: !Text
    , _ltfvVaultName :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListTagsForVault' smart constructor.
listTagsForVault :: Text -> Text -> ListTagsForVault
listTagsForVault pAccountId_ pVaultName_ =
    ListTagsForVault'
    { _ltfvAccountId = pAccountId_
    , _ltfvVaultName = pVaultName_
    }

-- | The @AccountId@ value is the AWS account ID of the account that owns the
-- vault. You can either specify an AWS account ID or optionally a single
-- apos@-@apos (hyphen), in which case Amazon Glacier uses the AWS account
-- ID associated with the credentials used to sign the request. If you use
-- an account ID, do not include any hyphens (apos-apos) in the ID.
ltfvAccountId :: Lens' ListTagsForVault Text
ltfvAccountId = lens _ltfvAccountId (\ s a -> s{_ltfvAccountId = a});

-- | The name of the vault.
ltfvVaultName :: Lens' ListTagsForVault Text
ltfvVaultName = lens _ltfvVaultName (\ s a -> s{_ltfvVaultName = a});

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
              ["/", toBS _ltfvAccountId, "/vaults/",
               toBS _ltfvVaultName, "/tags"]

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

-- | Undocumented member.
ltfvrsStatus :: Lens' ListTagsForVaultResponse Int
ltfvrsStatus = lens _ltfvrsStatus (\ s a -> s{_ltfvrsStatus = a});
