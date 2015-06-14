{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.ElasticTranscoder.DeletePreset
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

-- | The DeletePreset operation removes a preset that you\'ve added in an AWS
-- region.
--
-- You can\'t delete the default presets that are included with Elastic
-- Transcoder.
--
-- <http://docs.aws.amazon.com/elastictranscoder/latest/developerguide/DeletePreset.html>
module Network.AWS.ElasticTranscoder.DeletePreset
    (
    -- * Request
      DeletePreset
    -- ** Request constructor
    , deletePreset
    -- ** Request lenses
    , dpId

    -- * Response
    , DeletePresetResponse
    -- ** Response constructor
    , deletePresetResponse
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.ElasticTranscoder.Types

-- | /See:/ 'deletePreset' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dpId'
newtype DeletePreset = DeletePreset'{_dpId :: Text} deriving (Eq, Read, Show)

-- | 'DeletePreset' smart constructor.
deletePreset :: Text -> DeletePreset
deletePreset pId = DeletePreset'{_dpId = pId};

-- | The identifier of the preset for which you want to get detailed
-- information.
dpId :: Lens' DeletePreset Text
dpId = lens _dpId (\ s a -> s{_dpId = a});

instance AWSRequest DeletePreset where
        type Sv DeletePreset = ElasticTranscoder
        type Rs DeletePreset = DeletePresetResponse
        request = delete
        response = receiveNull DeletePresetResponse'

instance ToHeaders DeletePreset where
        toHeaders = const mempty

instance ToPath DeletePreset where
        toPath DeletePreset'{..}
          = mconcat ["/2012-09-25/presets/", toText _dpId]

instance ToQuery DeletePreset where
        toQuery = const mempty

-- | /See:/ 'deletePresetResponse' smart constructor.
data DeletePresetResponse = DeletePresetResponse' deriving (Eq, Read, Show)

-- | 'DeletePresetResponse' smart constructor.
deletePresetResponse :: DeletePresetResponse
deletePresetResponse = DeletePresetResponse';
