{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElasticTranscoder.DeletePreset
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The DeletePreset operation removes a preset that you've added in an AWS
-- region.
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

import Network.AWS.Prelude
import Network.AWS.Request.RestJSON
import Network.AWS.ElasticTranscoder.Types
import qualified GHC.Exts

newtype DeletePreset = DeletePreset
    { _dpId :: Text
    } deriving (Eq, Ord, Show, Monoid, IsString)

-- | 'DeletePreset' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dpId' @::@ 'Text'
--
deletePreset :: Text -- ^ 'dpId'
             -> DeletePreset
deletePreset p1 = DeletePreset
    { _dpId = p1
    }

-- | The identifier of the preset for which you want to get detailed
-- information.
dpId :: Lens' DeletePreset Text
dpId = lens _dpId (\s a -> s { _dpId = a })

data DeletePresetResponse = DeletePresetResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'DeletePresetResponse' constructor.
deletePresetResponse :: DeletePresetResponse
deletePresetResponse = DeletePresetResponse

instance ToPath DeletePreset where
    toPath DeletePreset{..} = mconcat
        [ "/2012-09-25/presets/"
        , toText _dpId
        ]

instance ToQuery DeletePreset where
    toQuery = const mempty

instance ToHeaders DeletePreset

instance ToJSON DeletePreset where
    toJSON = const (toJSON Empty)

json

instance AWSRequest DeletePreset where
    type Sv DeletePreset = ElasticTranscoder
    type Rs DeletePreset = DeletePresetResponse

    request  = delete
    response = nullResponse DeletePresetResponse
