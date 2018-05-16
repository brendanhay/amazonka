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
-- Module      : Network.AWS.Glacier.RemoveTagsFromVault
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation removes one or more tags from the set of tags attached to a vault. For more information about tags, see <http://docs.aws.amazon.com/amazonglacier/latest/dev/tagging.html Tagging Amazon Glacier Resources> . This operation is idempotent. The operation will be successful, even if there are no tags attached to the vault.
--
--
module Network.AWS.Glacier.RemoveTagsFromVault
    (
    -- * Creating a Request
      removeTagsFromVault
    , RemoveTagsFromVault
    -- * Request Lenses
    , rtfvTagKeys
    , rtfvAccountId
    , rtfvVaultName

    -- * Destructuring the Response
    , removeTagsFromVaultResponse
    , RemoveTagsFromVaultResponse
    ) where

import Network.AWS.Glacier.Types
import Network.AWS.Glacier.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | The input value for @RemoveTagsFromVaultInput@ .
--
--
--
-- /See:/ 'removeTagsFromVault' smart constructor.
data RemoveTagsFromVault = RemoveTagsFromVault'
  { _rtfvTagKeys   :: !(Maybe [Text])
  , _rtfvAccountId :: !Text
  , _rtfvVaultName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RemoveTagsFromVault' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rtfvTagKeys' - A list of tag keys. Each corresponding tag is removed from the vault.
--
-- * 'rtfvAccountId' - The @AccountId@ value is the AWS account ID of the account that owns the vault. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon Glacier uses the AWS account ID associated with the credentials used to sign the request. If you use an account ID, do not include any hyphens ('-') in the ID.
--
-- * 'rtfvVaultName' - The name of the vault.
removeTagsFromVault
    :: Text -- ^ 'rtfvAccountId'
    -> Text -- ^ 'rtfvVaultName'
    -> RemoveTagsFromVault
removeTagsFromVault pAccountId_ pVaultName_ =
  RemoveTagsFromVault'
    { _rtfvTagKeys = Nothing
    , _rtfvAccountId = pAccountId_
    , _rtfvVaultName = pVaultName_
    }


-- | A list of tag keys. Each corresponding tag is removed from the vault.
rtfvTagKeys :: Lens' RemoveTagsFromVault [Text]
rtfvTagKeys = lens _rtfvTagKeys (\ s a -> s{_rtfvTagKeys = a}) . _Default . _Coerce

-- | The @AccountId@ value is the AWS account ID of the account that owns the vault. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon Glacier uses the AWS account ID associated with the credentials used to sign the request. If you use an account ID, do not include any hyphens ('-') in the ID.
rtfvAccountId :: Lens' RemoveTagsFromVault Text
rtfvAccountId = lens _rtfvAccountId (\ s a -> s{_rtfvAccountId = a})

-- | The name of the vault.
rtfvVaultName :: Lens' RemoveTagsFromVault Text
rtfvVaultName = lens _rtfvVaultName (\ s a -> s{_rtfvVaultName = a})

instance AWSRequest RemoveTagsFromVault where
        type Rs RemoveTagsFromVault =
             RemoveTagsFromVaultResponse
        request = postJSON glacier
        response = receiveNull RemoveTagsFromVaultResponse'

instance Hashable RemoveTagsFromVault where

instance NFData RemoveTagsFromVault where

instance ToHeaders RemoveTagsFromVault where
        toHeaders = const mempty

instance ToJSON RemoveTagsFromVault where
        toJSON RemoveTagsFromVault'{..}
          = object
              (catMaybes [("TagKeys" .=) <$> _rtfvTagKeys])

instance ToPath RemoveTagsFromVault where
        toPath RemoveTagsFromVault'{..}
          = mconcat
              ["/", toBS _rtfvAccountId, "/vaults/",
               toBS _rtfvVaultName, "/tags"]

instance ToQuery RemoveTagsFromVault where
        toQuery = const (mconcat ["operation=remove"])

-- | /See:/ 'removeTagsFromVaultResponse' smart constructor.
data RemoveTagsFromVaultResponse =
  RemoveTagsFromVaultResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RemoveTagsFromVaultResponse' with the minimum fields required to make a request.
--
removeTagsFromVaultResponse
    :: RemoveTagsFromVaultResponse
removeTagsFromVaultResponse = RemoveTagsFromVaultResponse'


instance NFData RemoveTagsFromVaultResponse where
