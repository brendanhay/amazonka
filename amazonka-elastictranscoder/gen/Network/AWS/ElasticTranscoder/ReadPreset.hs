{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.ElasticTranscoder.ReadPreset
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

-- | The ReadPreset operation gets detailed information about a preset.
--
-- <http://docs.aws.amazon.com/elastictranscoder/latest/developerguide/ReadPreset.html>
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

import Network.AWS.ElasticTranscoder.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'readPreset' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rpId'
newtype ReadPreset = ReadPreset'{_rpId :: Text} deriving (Eq, Read, Show)

-- | 'ReadPreset' smart constructor.
readPreset :: Text -> ReadPreset
readPreset pId = ReadPreset'{_rpId = pId};

-- | The identifier of the preset for which you want to get detailed
-- information.
rpId :: Lens' ReadPreset Text
rpId = lens _rpId (\ s a -> s{_rpId = a});

instance AWSPager A where
        page rq rs
          | stop True = Nothing
          | otherwise = Just

instance AWSRequest ReadPreset where
        type Sv ReadPreset = ElasticTranscoder
        type Rs ReadPreset = ReadPresetResponse
        request = get
        response
          = receiveJSON
              (\ s h x -> ReadPresetResponse' <$> (x .?> "Preset"))

instance ToHeaders ReadPreset where
        toHeaders = const mempty

instance ToPath ReadPreset where
        toPath ReadPreset'{..}
          = mconcat ["/2012-09-25/presets/", toText _rpId]

instance ToQuery ReadPreset where
        toQuery = const mempty

-- | /See:/ 'readPresetResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rprPreset'
newtype ReadPresetResponse = ReadPresetResponse'{_rprPreset :: Maybe Preset} deriving (Eq, Read, Show)

-- | 'ReadPresetResponse' smart constructor.
readPresetResponse :: ReadPresetResponse
readPresetResponse = ReadPresetResponse'{_rprPreset = Nothing};

-- | A section of the response body that provides information about the
-- preset.
rprPreset :: Lens' ReadPresetResponse (Maybe Preset)
rprPreset = lens _rprPreset (\ s a -> s{_rprPreset = a});
