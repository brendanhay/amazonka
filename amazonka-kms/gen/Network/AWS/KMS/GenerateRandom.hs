{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

-- Module      : Network.AWS.KMS.GenerateRandom
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Generates an unpredictable byte string.
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

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.KMS.Types

newtype GenerateRandom = GenerateRandom
    { _grNumberOfBytes :: Maybe Natural
    } deriving (Eq, Ord, Show, Generic)

-- | 'GenerateRandom' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'grNumberOfBytes' @::@ 'Maybe' 'Natural'
--
generateRandom :: GenerateRandom
generateRandom = GenerateRandom
    { _grNumberOfBytes = Nothing
    }

-- | Integer that contains the number of bytes to generate. Common values are
-- 128, 256, 512, 1024 and so on. The current limit is 1024 bytes.
grNumberOfBytes :: Lens' GenerateRandom (Maybe Natural)
grNumberOfBytes = lens _grNumberOfBytes (\s a -> s { _grNumberOfBytes = a })

instance ToPath GenerateRandom where
    toPath = const "/"

instance ToQuery GenerateRandom where
    toQuery = const mempty

instance ToHeaders GenerateRandom

instance ToBody GenerateRandom where
    toBody = toBody . encode . _grNumberOfBytes

newtype GenerateRandomResponse = GenerateRandomResponse
    { _grrPlaintext :: Maybe Base64
    } deriving (Eq, Show, Generic)

-- | 'GenerateRandomResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'grrPlaintext' @::@ 'Maybe' 'Base64'
--
generateRandomResponse :: GenerateRandomResponse
generateRandomResponse = GenerateRandomResponse
    { _grrPlaintext = Nothing
    }

-- | Plaintext that contains the unpredictable byte string.
grrPlaintext :: Lens' GenerateRandomResponse (Maybe Base64)
grrPlaintext = lens _grrPlaintext (\s a -> s { _grrPlaintext = a })

-- FromJSON

instance AWSRequest GenerateRandom where
    type Sv GenerateRandom = KMS
    type Rs GenerateRandom = GenerateRandomResponse

    request  = post'
    response = jsonResponse $ \h o -> GenerateRandomResponse
        <$> o .: "Plaintext"
