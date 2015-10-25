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
-- Module      : Network.AWS.KMS.DisableKey
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the state of a master key to disabled, thereby preventing its use
-- for cryptographic operations. For more information about how key state
-- affects the use of a master key, go to
-- <http://docs.aws.amazon.com/kms/latest/developerguide/key-state.html How Key State Affects the Use of a Customer Master Key>
-- in the /AWS Key Management Service Developer Guide/.
--
-- /See:/ <http://docs.aws.amazon.com/kms/latest/APIReference/API_DisableKey.html AWS API Reference> for DisableKey.
module Network.AWS.KMS.DisableKey
    (
    -- * Creating a Request
      disableKey
    , DisableKey
    -- * Request Lenses
    , dkKeyId

    -- * Destructuring the Response
    , disableKeyResponse
    , DisableKeyResponse
    ) where

import           Network.AWS.KMS.Types
import           Network.AWS.KMS.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'disableKey' smart constructor.
newtype DisableKey = DisableKey'
    { _dkKeyId :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DisableKey' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dkKeyId'
disableKey
    :: Text -- ^ 'dkKeyId'
    -> DisableKey
disableKey pKeyId_ =
    DisableKey'
    { _dkKeyId = pKeyId_
    }

-- | A unique identifier for the customer master key. This value can be a
-- globally unique identifier or the fully specified ARN to a key.
--
-- -   Key ARN Example -
--     arn:aws:kms:us-east-1:123456789012:key\/12345678-1234-1234-1234-123456789012
-- -   Globally Unique Key ID Example -
--     12345678-1234-1234-1234-123456789012
dkKeyId :: Lens' DisableKey Text
dkKeyId = lens _dkKeyId (\ s a -> s{_dkKeyId = a});

instance AWSRequest DisableKey where
        type Rs DisableKey = DisableKeyResponse
        request = postJSON kMS
        response = receiveNull DisableKeyResponse'

instance ToHeaders DisableKey where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("TrentService.DisableKey" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DisableKey where
        toJSON DisableKey'{..}
          = object (catMaybes [Just ("KeyId" .= _dkKeyId)])

instance ToPath DisableKey where
        toPath = const "/"

instance ToQuery DisableKey where
        toQuery = const mempty

-- | /See:/ 'disableKeyResponse' smart constructor.
data DisableKeyResponse =
    DisableKeyResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DisableKeyResponse' with the minimum fields required to make a request.
--
disableKeyResponse
    :: DisableKeyResponse
disableKeyResponse = DisableKeyResponse'
