{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.KMS.EnableKeyRotation
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Enables rotation of the specified customer master key.
--
-- <http://docs.aws.amazon.com/kms/latest/APIReference/API_EnableKeyRotation.html>
module Network.AWS.KMS.EnableKeyRotation
    (
    -- * Request
      EnableKeyRotation
    -- ** Request constructor
    , enableKeyRotation
    -- ** Request lenses
    , ekrKeyId

    -- * Response
    , EnableKeyRotationResponse
    -- ** Response constructor
    , enableKeyRotationResponse
    ) where

import Network.AWS.KMS.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'enableKeyRotation' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ekrKeyId'
newtype EnableKeyRotation = EnableKeyRotation'{_ekrKeyId :: Text} deriving (Eq, Read, Show)

-- | 'EnableKeyRotation' smart constructor.
enableKeyRotation :: Text -> EnableKeyRotation
enableKeyRotation pKeyId = EnableKeyRotation'{_ekrKeyId = pKeyId};

-- | A unique identifier for the customer master key. This value can be a
-- globally unique identifier or the fully specified ARN to a key.
--
-- -   Key ARN Example -
--     arn:aws:kms:us-east-1:123456789012:key\/12345678-1234-1234-1234-123456789012
-- -   Globally Unique Key ID Example -
--     12345678-1234-1234-1234-123456789012
ekrKeyId :: Lens' EnableKeyRotation Text
ekrKeyId = lens _ekrKeyId (\ s a -> s{_ekrKeyId = a});

instance AWSRequest EnableKeyRotation where
        type Sv EnableKeyRotation = KMS
        type Rs EnableKeyRotation = EnableKeyRotationResponse
        request = postJSON
        response = receiveNull EnableKeyRotationResponse'

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
          = object ["KeyId" .= _ekrKeyId]

instance ToPath EnableKeyRotation where
        toPath = const "/"

instance ToQuery EnableKeyRotation where
        toQuery = const mempty

-- | /See:/ 'enableKeyRotationResponse' smart constructor.
data EnableKeyRotationResponse = EnableKeyRotationResponse' deriving (Eq, Read, Show)

-- | 'EnableKeyRotationResponse' smart constructor.
enableKeyRotationResponse :: EnableKeyRotationResponse
enableKeyRotationResponse = EnableKeyRotationResponse';
