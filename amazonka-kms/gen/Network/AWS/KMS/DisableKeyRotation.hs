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
-- Module      : Network.AWS.KMS.DisableKeyRotation
-- Copyright   : (c) 2013-2017 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables rotation of the specified key.
--
--
module Network.AWS.KMS.DisableKeyRotation
    (
    -- * Creating a Request
      disableKeyRotation
    , DisableKeyRotation
    -- * Request Lenses
    , dkrKeyId

    -- * Destructuring the Response
    , disableKeyRotationResponse
    , DisableKeyRotationResponse
    ) where

import Network.AWS.KMS.Types
import Network.AWS.KMS.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'disableKeyRotation' smart constructor.
newtype DisableKeyRotation = DisableKeyRotation'
  { _dkrKeyId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DisableKeyRotation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dkrKeyId' - A unique identifier for the customer master key. This value can be a globally unique identifier or the fully specified ARN to a key.     * Key ARN Example - arn:aws:kms:us-east-1:123456789012:key/12345678-1234-1234-1234-123456789012     * Globally Unique Key ID Example - 12345678-1234-1234-1234-123456789012
disableKeyRotation
    :: Text -- ^ 'dkrKeyId'
    -> DisableKeyRotation
disableKeyRotation pKeyId_ = DisableKeyRotation' {_dkrKeyId = pKeyId_}


-- | A unique identifier for the customer master key. This value can be a globally unique identifier or the fully specified ARN to a key.     * Key ARN Example - arn:aws:kms:us-east-1:123456789012:key/12345678-1234-1234-1234-123456789012     * Globally Unique Key ID Example - 12345678-1234-1234-1234-123456789012
dkrKeyId :: Lens' DisableKeyRotation Text
dkrKeyId = lens _dkrKeyId (\ s a -> s{_dkrKeyId = a});

instance AWSRequest DisableKeyRotation where
        type Rs DisableKeyRotation =
             DisableKeyRotationResponse
        request = postJSON kms
        response = receiveNull DisableKeyRotationResponse'

instance Hashable DisableKeyRotation where

instance NFData DisableKeyRotation where

instance ToHeaders DisableKeyRotation where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("TrentService.DisableKeyRotation" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DisableKeyRotation where
        toJSON DisableKeyRotation'{..}
          = object (catMaybes [Just ("KeyId" .= _dkrKeyId)])

instance ToPath DisableKeyRotation where
        toPath = const "/"

instance ToQuery DisableKeyRotation where
        toQuery = const mempty

-- | /See:/ 'disableKeyRotationResponse' smart constructor.
data DisableKeyRotationResponse =
  DisableKeyRotationResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DisableKeyRotationResponse' with the minimum fields required to make a request.
--
disableKeyRotationResponse
    :: DisableKeyRotationResponse
disableKeyRotationResponse = DisableKeyRotationResponse'


instance NFData DisableKeyRotationResponse where
