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

-- Module      : Network.AWS.KMS.EnableKeyRotation
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Enables rotation of the specified customer master key.
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

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.KMS.Types

newtype EnableKeyRotation = EnableKeyRotation
    { _ekrKeyId :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid, IsString)

-- | 'EnableKeyRotation' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ekrKeyId' @::@ 'Text'
--
enableKeyRotation :: Text -- ^ 'ekrKeyId'
                  -> EnableKeyRotation
enableKeyRotation p1 = EnableKeyRotation
    { _ekrKeyId = p1
    }

-- | Unique identifier of the customer master key for which rotation is to be
-- enabled. This can be an ARN, an alias, or a globally unique identifier.
ekrKeyId :: Lens' EnableKeyRotation Text
ekrKeyId = lens _ekrKeyId (\s a -> s { _ekrKeyId = a })

instance ToPath EnableKeyRotation where
    toPath = const "/"

instance ToQuery EnableKeyRotation where
    toQuery = const mempty

instance ToHeaders EnableKeyRotation

instance ToBody EnableKeyRotation where
    toBody = toBody . encode . _ekrKeyId

data EnableKeyRotationResponse = EnableKeyRotationResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'EnableKeyRotationResponse' constructor.
enableKeyRotationResponse :: EnableKeyRotationResponse
enableKeyRotationResponse = EnableKeyRotationResponse

instance AWSRequest EnableKeyRotation where
    type Sv EnableKeyRotation = KMS
    type Rs EnableKeyRotation = EnableKeyRotationResponse

    request  = post
    response = nullaryResponse EnableKeyRotationResponse
