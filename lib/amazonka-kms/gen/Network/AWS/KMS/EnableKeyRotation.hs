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
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables automatic rotation of the key material for the specified customer master key (CMK). You cannot perform this operation on a CMK in a different AWS account.
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
-- * 'ekrKeyId' - A unique identifier for the customer master key (CMK). Specify the key ID or the Amazon Resource Name (ARN) of the CMK. For example:     * Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@      * Key ARN: @arn:aws:kms:us-east-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab@  To get the key ID and key ARN for a CMK, use 'ListKeys' or 'DescribeKey' .
enableKeyRotation
    :: Text -- ^ 'ekrKeyId'
    -> EnableKeyRotation
enableKeyRotation pKeyId_ = EnableKeyRotation' {_ekrKeyId = pKeyId_}


-- | A unique identifier for the customer master key (CMK). Specify the key ID or the Amazon Resource Name (ARN) of the CMK. For example:     * Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@      * Key ARN: @arn:aws:kms:us-east-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab@  To get the key ID and key ARN for a CMK, use 'ListKeys' or 'DescribeKey' .
ekrKeyId :: Lens' EnableKeyRotation Text
ekrKeyId = lens _ekrKeyId (\ s a -> s{_ekrKeyId = a})

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
