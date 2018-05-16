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
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables automatic rotation of the key material for the specified customer master key (CMK). You cannot perform this operation on a CMK in a different AWS account.
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
-- * 'dkrKeyId' - A unique identifier for the customer master key (CMK). Specify the key ID or the Amazon Resource Name (ARN) of the CMK. For example:     * Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@      * Key ARN: @arn:aws:kms:us-east-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab@  To get the key ID and key ARN for a CMK, use 'ListKeys' or 'DescribeKey' .
disableKeyRotation
    :: Text -- ^ 'dkrKeyId'
    -> DisableKeyRotation
disableKeyRotation pKeyId_ = DisableKeyRotation' {_dkrKeyId = pKeyId_}


-- | A unique identifier for the customer master key (CMK). Specify the key ID or the Amazon Resource Name (ARN) of the CMK. For example:     * Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@      * Key ARN: @arn:aws:kms:us-east-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab@  To get the key ID and key ARN for a CMK, use 'ListKeys' or 'DescribeKey' .
dkrKeyId :: Lens' DisableKeyRotation Text
dkrKeyId = lens _dkrKeyId (\ s a -> s{_dkrKeyId = a})

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
