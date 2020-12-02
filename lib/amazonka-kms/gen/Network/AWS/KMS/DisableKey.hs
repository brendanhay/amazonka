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
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the state of a customer master key (CMK) to disabled, thereby preventing its use for cryptographic operations. You cannot perform this operation on a CMK in a different AWS account.
--
--
-- For more information about how key state affects the use of a CMK, see <http://docs.aws.amazon.com/kms/latest/developerguide/key-state.html How Key State Affects the Use of a Customer Master Key> in the /AWS Key Management Service Developer Guide/ .
--
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

import Network.AWS.KMS.Types
import Network.AWS.KMS.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'disableKey' smart constructor.
newtype DisableKey = DisableKey'
  { _dkKeyId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DisableKey' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dkKeyId' - A unique identifier for the customer master key (CMK). Specify the key ID or the Amazon Resource Name (ARN) of the CMK. For example:     * Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@      * Key ARN: @arn:aws:kms:us-east-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab@  To get the key ID and key ARN for a CMK, use 'ListKeys' or 'DescribeKey' .
disableKey
    :: Text -- ^ 'dkKeyId'
    -> DisableKey
disableKey pKeyId_ = DisableKey' {_dkKeyId = pKeyId_}


-- | A unique identifier for the customer master key (CMK). Specify the key ID or the Amazon Resource Name (ARN) of the CMK. For example:     * Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@      * Key ARN: @arn:aws:kms:us-east-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab@  To get the key ID and key ARN for a CMK, use 'ListKeys' or 'DescribeKey' .
dkKeyId :: Lens' DisableKey Text
dkKeyId = lens _dkKeyId (\ s a -> s{_dkKeyId = a})

instance AWSRequest DisableKey where
        type Rs DisableKey = DisableKeyResponse
        request = postJSON kms
        response = receiveNull DisableKeyResponse'

instance Hashable DisableKey where

instance NFData DisableKey where

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
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DisableKeyResponse' with the minimum fields required to make a request.
--
disableKeyResponse
    :: DisableKeyResponse
disableKeyResponse = DisableKeyResponse'


instance NFData DisableKeyResponse where
