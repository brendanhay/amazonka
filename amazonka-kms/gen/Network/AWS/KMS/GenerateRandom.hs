{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.KMS.GenerateRandom
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

-- | Generates an unpredictable byte string.
--
-- <http://docs.aws.amazon.com/kms/latest/APIReference/API_GenerateRandom.html>
module Network.AWS.KMS.GenerateRandom
    (
    -- * Request
      GenerateRandom
    -- ** Request constructor
    , generateRandom
    -- ** Request lenses
    , grNumberOfBytes

    -- * Response
    , GenerateRandomResponse
    -- ** Response constructor
    , generateRandomResponse
    -- ** Response lenses
    , grrPlaintext
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.KMS.Types

-- | /See:/ 'generateRandom' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'grNumberOfBytes'
newtype GenerateRandom = GenerateRandom'{_grNumberOfBytes :: Maybe Nat} deriving (Eq, Read, Show)

-- | 'GenerateRandom' smart constructor.
generateRandom :: GenerateRandom
generateRandom = GenerateRandom'{_grNumberOfBytes = Nothing};

-- | Integer that contains the number of bytes to generate. Common values are
-- 128, 256, 512, 1024 and so on. The current limit is 1024 bytes.
grNumberOfBytes :: Lens' GenerateRandom (Maybe Natural)
grNumberOfBytes = lens _grNumberOfBytes (\ s a -> s{_grNumberOfBytes = a}) . mapping _Nat;

instance AWSRequest GenerateRandom where
        type Sv GenerateRandom = KMS
        type Rs GenerateRandom = GenerateRandomResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 GenerateRandomResponse' <$> (x .?> "Plaintext"))

instance ToHeaders GenerateRandom where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("TrentService.GenerateRandom" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GenerateRandom where
        toJSON GenerateRandom'{..}
          = object ["NumberOfBytes" .= _grNumberOfBytes]

instance ToPath GenerateRandom where
        toPath = const "/"

instance ToQuery GenerateRandom where
        toQuery = const mempty

-- | /See:/ 'generateRandomResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'grrPlaintext'
newtype GenerateRandomResponse = GenerateRandomResponse'{_grrPlaintext :: Maybe (Sensitive Base64)} deriving (Eq, Read, Show)

-- | 'GenerateRandomResponse' smart constructor.
generateRandomResponse :: GenerateRandomResponse
generateRandomResponse = GenerateRandomResponse'{_grrPlaintext = Nothing};

-- | Plaintext that contains the unpredictable byte string.
grrPlaintext :: Lens' GenerateRandomResponse (Maybe Base64)
grrPlaintext = lens _grrPlaintext (\ s a -> s{_grrPlaintext = a}) . mapping _Sensitive;
