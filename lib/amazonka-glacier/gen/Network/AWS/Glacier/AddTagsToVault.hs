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
-- Module      : Network.AWS.Glacier.AddTagsToVault
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation adds the specified tags to a vault. Each tag is composed of a key and a value. Each vault can have up to 10 tags. If your request would cause the tag limit for the vault to be exceeded, the operation throws the @LimitExceededException@ error. If a tag already exists on the vault under a specified key, the existing key value will be overwritten. For more information about tags, see <http://docs.aws.amazon.com/amazonglacier/latest/dev/tagging.html Tagging Amazon Glacier Resources> .
--
--
module Network.AWS.Glacier.AddTagsToVault
    (
    -- * Creating a Request
      addTagsToVault
    , AddTagsToVault
    -- * Request Lenses
    , attvTags
    , attvAccountId
    , attvVaultName

    -- * Destructuring the Response
    , addTagsToVaultResponse
    , AddTagsToVaultResponse
    ) where

import Network.AWS.Glacier.Types
import Network.AWS.Glacier.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | The input values for @AddTagsToVault@ .
--
--
--
-- /See:/ 'addTagsToVault' smart constructor.
data AddTagsToVault = AddTagsToVault'
  { _attvTags      :: !(Maybe (Map Text Text))
  , _attvAccountId :: !Text
  , _attvVaultName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AddTagsToVault' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'attvTags' - The tags to add to the vault. Each tag is composed of a key and a value. The value can be an empty string.
--
-- * 'attvAccountId' - The @AccountId@ value is the AWS account ID of the account that owns the vault. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon Glacier uses the AWS account ID associated with the credentials used to sign the request. If you use an account ID, do not include any hyphens ('-') in the ID.
--
-- * 'attvVaultName' - The name of the vault.
addTagsToVault
    :: Text -- ^ 'attvAccountId'
    -> Text -- ^ 'attvVaultName'
    -> AddTagsToVault
addTagsToVault pAccountId_ pVaultName_ =
  AddTagsToVault'
    { _attvTags = Nothing
    , _attvAccountId = pAccountId_
    , _attvVaultName = pVaultName_
    }


-- | The tags to add to the vault. Each tag is composed of a key and a value. The value can be an empty string.
attvTags :: Lens' AddTagsToVault (HashMap Text Text)
attvTags = lens _attvTags (\ s a -> s{_attvTags = a}) . _Default . _Map

-- | The @AccountId@ value is the AWS account ID of the account that owns the vault. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon Glacier uses the AWS account ID associated with the credentials used to sign the request. If you use an account ID, do not include any hyphens ('-') in the ID.
attvAccountId :: Lens' AddTagsToVault Text
attvAccountId = lens _attvAccountId (\ s a -> s{_attvAccountId = a})

-- | The name of the vault.
attvVaultName :: Lens' AddTagsToVault Text
attvVaultName = lens _attvVaultName (\ s a -> s{_attvVaultName = a})

instance AWSRequest AddTagsToVault where
        type Rs AddTagsToVault = AddTagsToVaultResponse
        request = postJSON glacier
        response = receiveNull AddTagsToVaultResponse'

instance Hashable AddTagsToVault where

instance NFData AddTagsToVault where

instance ToHeaders AddTagsToVault where
        toHeaders = const mempty

instance ToJSON AddTagsToVault where
        toJSON AddTagsToVault'{..}
          = object (catMaybes [("Tags" .=) <$> _attvTags])

instance ToPath AddTagsToVault where
        toPath AddTagsToVault'{..}
          = mconcat
              ["/", toBS _attvAccountId, "/vaults/",
               toBS _attvVaultName, "/tags"]

instance ToQuery AddTagsToVault where
        toQuery = const (mconcat ["operation=add"])

-- | /See:/ 'addTagsToVaultResponse' smart constructor.
data AddTagsToVaultResponse =
  AddTagsToVaultResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AddTagsToVaultResponse' with the minimum fields required to make a request.
--
addTagsToVaultResponse
    :: AddTagsToVaultResponse
addTagsToVaultResponse = AddTagsToVaultResponse'


instance NFData AddTagsToVaultResponse where
