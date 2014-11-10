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

-- Module      : Network.AWS.ElasticBeanstalk.UpdateApplicationVersion
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Updates the specified application version to have the specified properties.
module Network.AWS.ElasticBeanstalk.UpdateApplicationVersion
    (
    -- * Request
      UpdateApplicationVersionMessage
    -- ** Request constructor
    , updateApplicationVersion
    -- ** Request lenses
    , uavmApplicationName
    , uavmDescription
    , uavmVersionLabel

    -- * Response
    , ApplicationVersionDescriptionMessage
    -- ** Response constructor
    , updateApplicationVersionResponse
    -- ** Response lenses
    , avdmApplicationVersion
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ElasticBeanstalk.Types

data UpdateApplicationVersionMessage = UpdateApplicationVersionMessage
    { _uavmApplicationName :: Text
    , _uavmDescription     :: Maybe Text
    , _uavmVersionLabel    :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'UpdateApplicationVersionMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'uavmApplicationName' @::@ 'Text'
--
-- * 'uavmDescription' @::@ 'Maybe' 'Text'
--
-- * 'uavmVersionLabel' @::@ 'Text'
--
updateApplicationVersion :: Text -- ^ 'uavmApplicationName'
                         -> Text -- ^ 'uavmVersionLabel'
                         -> UpdateApplicationVersionMessage
updateApplicationVersion p1 p2 = UpdateApplicationVersionMessage
    { _uavmApplicationName = p1
    , _uavmVersionLabel    = p2
    , _uavmDescription     = Nothing
    }

-- | The name of the application associated with this version. If no
-- application is found with this name, UpdateApplication returns an
-- InvalidParameterValue error.
uavmApplicationName :: Lens' UpdateApplicationVersionMessage Text
uavmApplicationName =
    lens _uavmApplicationName (\s a -> s { _uavmApplicationName = a })

-- | A new description for this release.
uavmDescription :: Lens' UpdateApplicationVersionMessage (Maybe Text)
uavmDescription = lens _uavmDescription (\s a -> s { _uavmDescription = a })

-- | The name of the version to update. If no application version is found
-- with this label, UpdateApplication returns an InvalidParameterValue
-- error.
uavmVersionLabel :: Lens' UpdateApplicationVersionMessage Text
uavmVersionLabel = lens _uavmVersionLabel (\s a -> s { _uavmVersionLabel = a })

instance ToPath UpdateApplicationVersionMessage where
    toPath = const "/"

instance ToQuery UpdateApplicationVersionMessage

instance AWSRequest UpdateApplicationVersionMessage where
    type Sv UpdateApplicationVersionMessage = ElasticBeanstalk
    type Rs UpdateApplicationVersionMessage = ApplicationVersionDescriptionMessage

    request  = post "UpdateApplicationVersion"
    response = xmlResponse $ const decodeCursor
