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
-- Module      : Network.AWS.KMS.EnableKeyRotation
-- Copyright   : (c) 2013-2017 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables rotation of the specified customer master key.
--
--
module Network.AWS.KMS.EnableKeyRotation
    (
    -- * Creating a Request
      enableKeyRotation
    , EnableKeyRotation
    -- * Request Lenses
    , ekrKeyId

    -- * Destructuring the Response
    , enableKeyRotationResponse
    , EnableKeyRotationResponse
    ) where

import Network.AWS.KMS.Types
import Network.AWS.KMS.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'enableKeyRotation' smart constructor.
newtype EnableKeyRotation = EnableKeyRotation'
  { _ekrKeyId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'EnableKeyRotation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ekrKeyId' - A unique identifier for the customer master key. This value can be a globally unique identifier or the fully specified ARN to a key.     * Key ARN Example - arn:aws:kms:us-east-1:123456789012:key/12345678-1234-1234-1234-123456789012     * Globally Unique Key ID Example - 12345678-1234-1234-1234-123456789012
enableKeyRotation
    :: Text -- ^ 'ekrKeyId'
    -> EnableKeyRotation
enableKeyRotation pKeyId_ = EnableKeyRotation' {_ekrKeyId = pKeyId_}


-- | A unique identifier for the customer master key. This value can be a globally unique identifier or the fully specified ARN to a key.     * Key ARN Example - arn:aws:kms:us-east-1:123456789012:key/12345678-1234-1234-1234-123456789012     * Globally Unique Key ID Example - 12345678-1234-1234-1234-123456789012
ekrKeyId :: Lens' EnableKeyRotation Text
ekrKeyId = lens _ekrKeyId (\ s a -> s{_ekrKeyId = a});

instance AWSRequest EnableKeyRotation where
        type Rs EnableKeyRotation = EnableKeyRotationResponse
        request = postJSON kms
        response = receiveNull EnableKeyRotationResponse'

instance Hashable EnableKeyRotation where

instance NFData EnableKeyRotation where

instance ToHeaders EnableKeyRotation where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("TrentService.EnableKeyRotation" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON EnableKeyRotation where
        toJSON EnableKeyRotation'{..}
          = object (catMaybes [Just ("KeyId" .= _ekrKeyId)])

instance ToPath EnableKeyRotation where
        toPath = const "/"

instance ToQuery EnableKeyRotation where
        toQuery = const mempty

-- | /See:/ 'enableKeyRotationResponse' smart constructor.
data EnableKeyRotationResponse =
  EnableKeyRotationResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'EnableKeyRotationResponse' with the minimum fields required to make a request.
--
enableKeyRotationResponse
    :: EnableKeyRotationResponse
enableKeyRotationResponse = EnableKeyRotationResponse'


instance NFData EnableKeyRotationResponse where
