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
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the state of a customer master key (CMK) to enabled, thereby permitting its use for cryptographic operations. You cannot perform this operation on a CMK in a different AWS account.
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
-- * 'ekKeyId' - A unique identifier for the customer master key (CMK). Specify the key ID or the Amazon Resource Name (ARN) of the CMK. For example:     * Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@      * Key ARN: @arn:aws:kms:us-east-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab@  To get the key ID and key ARN for a CMK, use 'ListKeys' or 'DescribeKey' .
enableKey
    :: Text -- ^ 'ekKeyId'
    -> EnableKey
enableKey pKeyId_ = EnableKey' {_ekKeyId = pKeyId_}


-- | A unique identifier for the customer master key (CMK). Specify the key ID or the Amazon Resource Name (ARN) of the CMK. For example:     * Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@      * Key ARN: @arn:aws:kms:us-east-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab@  To get the key ID and key ARN for a CMK, use 'ListKeys' or 'DescribeKey' .
ekKeyId :: Lens' EnableKey Text
ekKeyId = lens _ekKeyId (\ s a -> s{_ekKeyId = a})

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
