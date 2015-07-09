{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.RemoveTagsFromVault
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- This operation removes one or more tags from the set of tags attached to
-- a vault. For more information about tags, see
-- <http://docs.aws.amazon.com/amazonglacier/latest/dev/tagging.html Tagging Amazon Glacier Resources>.
-- This operation is idempotent. The operation will be successful, even if
-- there are no tags attached to the vault.
--
-- <http://docs.aws.amazon.com/amazonglacier/latest/dev/api-RemoveTagsFromVault.html>
module Network.AWS.Glacier.RemoveTagsFromVault
    (
    -- * Request
      RemoveTagsFromVault
    -- ** Request constructor
    , removeTagsFromVault
    -- ** Request lenses
    , rtfvTagKeys
    , rtfvAccountId
    , rtfvVaultName

    -- * Response
    , RemoveTagsFromVaultResponse
    -- ** Response constructor
    , removeTagsFromVaultResponse
    ) where

import           Network.AWS.Glacier.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | The input value for @RemoveTagsFromVaultInput@.
--
-- /See:/ 'removeTagsFromVault' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rtfvTagKeys'
--
-- * 'rtfvAccountId'
--
-- * 'rtfvVaultName'
data RemoveTagsFromVault = RemoveTagsFromVault'
    { _rtfvTagKeys   :: !(Maybe [Text])
    , _rtfvAccountId :: !Text
    , _rtfvVaultName :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'RemoveTagsFromVault' smart constructor.
removeTagsFromVault :: Text -> Text -> RemoveTagsFromVault
removeTagsFromVault pAccountId pVaultName =
    RemoveTagsFromVault'
    { _rtfvTagKeys = Nothing
    , _rtfvAccountId = pAccountId
    , _rtfvVaultName = pVaultName
    }

-- | A list of tag keys. Each corresponding tag is removed from the vault.
rtfvTagKeys :: Lens' RemoveTagsFromVault [Text]
rtfvTagKeys = lens _rtfvTagKeys (\ s a -> s{_rtfvTagKeys = a}) . _Default;

-- | The @AccountId@ value is the AWS account ID of the account that owns the
-- vault. You can either specify an AWS account ID or optionally a single
-- apos@-@apos (hyphen), in which case Amazon Glacier uses the AWS account
-- ID associated with the credentials used to sign the request. If you use
-- an account ID, do not include any hyphens (apos-apos) in the ID.
rtfvAccountId :: Lens' RemoveTagsFromVault Text
rtfvAccountId = lens _rtfvAccountId (\ s a -> s{_rtfvAccountId = a});

-- | The name of the vault.
rtfvVaultName :: Lens' RemoveTagsFromVault Text
rtfvVaultName = lens _rtfvVaultName (\ s a -> s{_rtfvVaultName = a});

instance AWSRequest RemoveTagsFromVault where
        type Sv RemoveTagsFromVault = Glacier
        type Rs RemoveTagsFromVault =
             RemoveTagsFromVaultResponse
        request = postJSON
        response = receiveNull RemoveTagsFromVaultResponse'

instance ToHeaders RemoveTagsFromVault where
        toHeaders = const mempty

instance ToJSON RemoveTagsFromVault where
        toJSON RemoveTagsFromVault'{..}
          = object ["TagKeys" .= _rtfvTagKeys]

instance ToPath RemoveTagsFromVault where
        toPath RemoveTagsFromVault'{..}
          = mconcat
              ["/", toText _rtfvAccountId, "/vaults/",
               toText _rtfvVaultName, "/tags"]

instance ToQuery RemoveTagsFromVault where
        toQuery = const (mconcat ["operation=remove"])

-- | /See:/ 'removeTagsFromVaultResponse' smart constructor.
data RemoveTagsFromVaultResponse =
    RemoveTagsFromVaultResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'RemoveTagsFromVaultResponse' smart constructor.
removeTagsFromVaultResponse :: RemoveTagsFromVaultResponse
removeTagsFromVaultResponse = RemoveTagsFromVaultResponse'
