{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.AddTagsToVault
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- This operation adds the specified tags to a vault. Each tag is composed
-- of a key and a value. Each vault can have up to 10 tags. If your request
-- would cause the tag limit for the vault to be exceeded, the operation
-- throws the @LimitExceededException@ error. If a tag already exists on
-- the vault under a specified key, the existing key value will be
-- overwritten. For more information about tags, see
-- <http://docs.aws.amazon.com/amazonglacier/latest/dev/tagging.html Tagging Amazon Glacier Resources>.
--
-- <http://docs.aws.amazon.com/amazonglacier/latest/dev/api-AddTagsToVault.html>
module Network.AWS.Glacier.AddTagsToVault
    (
    -- * Request
      AddTagsToVault
    -- ** Request constructor
    , addTagsToVault
    -- ** Request lenses
    , attvrqTags
    , attvrqAccountId
    , attvrqVaultName

    -- * Response
    , AddTagsToVaultResponse
    -- ** Response constructor
    , addTagsToVaultResponse
    ) where

import           Network.AWS.Glacier.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | The input value for @AddTagsToVault@.
--
-- /See:/ 'addTagsToVault' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'attvrqTags'
--
-- * 'attvrqAccountId'
--
-- * 'attvrqVaultName'
data AddTagsToVault = AddTagsToVault'
    { _attvrqTags      :: !(Maybe (Map Text Text))
    , _attvrqAccountId :: !Text
    , _attvrqVaultName :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'AddTagsToVault' smart constructor.
addTagsToVault :: Text -> Text -> AddTagsToVault
addTagsToVault pAccountId_ pVaultName_ =
    AddTagsToVault'
    { _attvrqTags = Nothing
    , _attvrqAccountId = pAccountId_
    , _attvrqVaultName = pVaultName_
    }

-- | The tags to add to the vault. Each tag is composed of a key and a value.
-- The value can be an empty string.
attvrqTags :: Lens' AddTagsToVault (HashMap Text Text)
attvrqTags = lens _attvrqTags (\ s a -> s{_attvrqTags = a}) . _Default . _Map;

-- | The @AccountId@ value is the AWS account ID of the account that owns the
-- vault. You can either specify an AWS account ID or optionally a single
-- apos@-@apos (hyphen), in which case Amazon Glacier uses the AWS account
-- ID associated with the credentials used to sign the request. If you use
-- an account ID, do not include any hyphens (apos-apos) in the ID.
attvrqAccountId :: Lens' AddTagsToVault Text
attvrqAccountId = lens _attvrqAccountId (\ s a -> s{_attvrqAccountId = a});

-- | The name of the vault.
attvrqVaultName :: Lens' AddTagsToVault Text
attvrqVaultName = lens _attvrqVaultName (\ s a -> s{_attvrqVaultName = a});

instance AWSRequest AddTagsToVault where
        type Sv AddTagsToVault = Glacier
        type Rs AddTagsToVault = AddTagsToVaultResponse
        request = postJSON
        response = receiveNull AddTagsToVaultResponse'

instance ToHeaders AddTagsToVault where
        toHeaders = const mempty

instance ToJSON AddTagsToVault where
        toJSON AddTagsToVault'{..}
          = object ["Tags" .= _attvrqTags]

instance ToPath AddTagsToVault where
        toPath AddTagsToVault'{..}
          = mconcat
              ["/", toText _attvrqAccountId, "/vaults/",
               toText _attvrqVaultName, "/tags"]

instance ToQuery AddTagsToVault where
        toQuery = const (mconcat ["operation=add"])

-- | /See:/ 'addTagsToVaultResponse' smart constructor.
data AddTagsToVaultResponse =
    AddTagsToVaultResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'AddTagsToVaultResponse' smart constructor.
addTagsToVaultResponse :: AddTagsToVaultResponse
addTagsToVaultResponse = AddTagsToVaultResponse'
