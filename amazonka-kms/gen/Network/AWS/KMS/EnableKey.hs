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
-- Module      : Network.AWS.KMS.EnableKey
-- Copyright   : (c) 2013-2017 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Marks a key as enabled, thereby permitting its use.
--
--
module Network.AWS.KMS.EnableKey
    (
    -- * Creating a Request
      enableKey
    , EnableKey
    -- * Request Lenses
    , ekKeyId

    -- * Destructuring the Response
    , enableKeyResponse
    , EnableKeyResponse
    ) where

import Network.AWS.KMS.Types
import Network.AWS.KMS.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'enableKey' smart constructor.
newtype EnableKey = EnableKey'
  { _ekKeyId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'EnableKey' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ekKeyId' - A unique identifier for the customer master key. This value can be a globally unique identifier or the fully specified ARN to a key.     * Key ARN Example - arn:aws:kms:us-east-1:123456789012:key/12345678-1234-1234-1234-123456789012     * Globally Unique Key ID Example - 12345678-1234-1234-1234-123456789012
enableKey
    :: Text -- ^ 'ekKeyId'
    -> EnableKey
enableKey pKeyId_ = EnableKey' {_ekKeyId = pKeyId_}


-- | A unique identifier for the customer master key. This value can be a globally unique identifier or the fully specified ARN to a key.     * Key ARN Example - arn:aws:kms:us-east-1:123456789012:key/12345678-1234-1234-1234-123456789012     * Globally Unique Key ID Example - 12345678-1234-1234-1234-123456789012
ekKeyId :: Lens' EnableKey Text
ekKeyId = lens _ekKeyId (\ s a -> s{_ekKeyId = a});

instance AWSRequest EnableKey where
        type Rs EnableKey = EnableKeyResponse
        request = postJSON kms
        response = receiveNull EnableKeyResponse'

instance Hashable EnableKey where

instance NFData EnableKey where

instance ToHeaders EnableKey where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("TrentService.EnableKey" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON EnableKey where
        toJSON EnableKey'{..}
          = object (catMaybes [Just ("KeyId" .= _ekKeyId)])

instance ToPath EnableKey where
        toPath = const "/"

instance ToQuery EnableKey where
        toQuery = const mempty

-- | /See:/ 'enableKeyResponse' smart constructor.
data EnableKeyResponse =
  EnableKeyResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'EnableKeyResponse' with the minimum fields required to make a request.
--
enableKeyResponse
    :: EnableKeyResponse
enableKeyResponse = EnableKeyResponse'


instance NFData EnableKeyResponse where
