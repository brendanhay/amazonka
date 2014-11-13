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

-- Module      : Network.AWS.ElasticTranscoder.ReadPreset
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The ReadPreset operation gets detailed information about a preset.
module Network.AWS.ElasticTranscoder.ReadPreset
    (
    -- * Request
      ReadPreset
    -- ** Request constructor
    , readPreset
    -- ** Request lenses
    , rpId

    -- * Response
    , ReadPresetResponse
    -- ** Response constructor
    , readPresetResponse
    -- ** Response lenses
    , rprPreset
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.ElasticTranscoder.Types

newtype ReadPreset = ReadPreset
    { _rpId :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid, IsString)

-- | 'ReadPreset' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rpId' @::@ 'Text'
--
readPreset :: Text -- ^ 'rpId'
           -> ReadPreset
readPreset p1 = ReadPreset
    { _rpId = p1
    }

-- | The identifier of the preset for which you want to get detailed
-- information.
rpId :: Lens' ReadPreset Text
rpId = lens _rpId (\s a -> s { _rpId = a })

instance ToPath ReadPreset where
    toPath ReadPreset{..} = mconcat
        [ "/2012-09-25/presets/"
        , toText _rpId
        ]

instance ToQuery ReadPreset where
    toQuery = const mempty

instance ToHeaders ReadPreset

newtype ReadPresetResponse = ReadPresetResponse
    { _rprPreset :: Maybe Preset
    } deriving (Eq, Show, Generic)

-- | 'ReadPresetResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rprPreset' @::@ 'Maybe' 'Preset'
--
readPresetResponse :: ReadPresetResponse
readPresetResponse = ReadPresetResponse
    { _rprPreset = Nothing
    }

-- | A section of the response body that provides information about the
-- preset.
rprPreset :: Lens' ReadPresetResponse (Maybe Preset)
rprPreset = lens _rprPreset (\s a -> s { _rprPreset = a })

-- FromJSON

instance AWSRequest ReadPreset where
    type Sv ReadPreset = ElasticTranscoder
    type Rs ReadPreset = ReadPresetResponse

    request  = get'
    response = jsonResponse $ \h o -> ReadPresetResponse
        <$> o .: "Preset"
