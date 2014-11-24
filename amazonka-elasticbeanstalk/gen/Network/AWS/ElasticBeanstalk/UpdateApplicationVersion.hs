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
--
-- <http://docs.aws.amazon.com/elasticbeanstalk/latest/api/API_UpdateApplicationVersion.html>
module Network.AWS.ElasticBeanstalk.UpdateApplicationVersion
    (
    -- * Request
      UpdateApplicationVersion
    -- ** Request constructor
    , updateApplicationVersion
    -- ** Request lenses
    , uavApplicationName
    , uavDescription
    , uavVersionLabel

    -- * Response
    , UpdateApplicationVersionResponse
    -- ** Response constructor
    , updateApplicationVersionResponse
    -- ** Response lenses
    , uavrApplicationVersion
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ElasticBeanstalk.Types
import qualified GHC.Exts

data UpdateApplicationVersion = UpdateApplicationVersion
    { _uavApplicationName :: Text
    , _uavDescription     :: Maybe Text
    , _uavVersionLabel    :: Text
    } deriving (Eq, Ord, Show)

-- | 'UpdateApplicationVersion' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'uavApplicationName' @::@ 'Text'
--
-- * 'uavDescription' @::@ 'Maybe' 'Text'
--
-- * 'uavVersionLabel' @::@ 'Text'
--
updateApplicationVersion :: Text -- ^ 'uavApplicationName'
                         -> Text -- ^ 'uavVersionLabel'
                         -> UpdateApplicationVersion
updateApplicationVersion p1 p2 = UpdateApplicationVersion
    { _uavApplicationName = p1
    , _uavVersionLabel    = p2
    , _uavDescription     = Nothing
    }

-- | The name of the application associated with this version. If no
-- application is found with this name, @UpdateApplication@ returns an
-- @InvalidParameterValue@ error.
uavApplicationName :: Lens' UpdateApplicationVersion Text
uavApplicationName =
    lens _uavApplicationName (\s a -> s { _uavApplicationName = a })

-- | A new description for this release.
uavDescription :: Lens' UpdateApplicationVersion (Maybe Text)
uavDescription = lens _uavDescription (\s a -> s { _uavDescription = a })

-- | The name of the version to update. If no application version is found
-- with this label, @UpdateApplication@ returns an @InvalidParameterValue@
-- error.
uavVersionLabel :: Lens' UpdateApplicationVersion Text
uavVersionLabel = lens _uavVersionLabel (\s a -> s { _uavVersionLabel = a })

newtype UpdateApplicationVersionResponse = UpdateApplicationVersionResponse
    { _uavrApplicationVersion :: Maybe ApplicationVersionDescription
    } deriving (Eq, Show)

-- | 'UpdateApplicationVersionResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'uavrApplicationVersion' @::@ 'Maybe' 'ApplicationVersionDescription'
--
updateApplicationVersionResponse :: UpdateApplicationVersionResponse
updateApplicationVersionResponse = UpdateApplicationVersionResponse
    { _uavrApplicationVersion = Nothing
    }

-- | The 'ApplicationVersionDescription' of the application version.
uavrApplicationVersion :: Lens' UpdateApplicationVersionResponse (Maybe ApplicationVersionDescription)
uavrApplicationVersion =
    lens _uavrApplicationVersion (\s a -> s { _uavrApplicationVersion = a })

instance ToPath UpdateApplicationVersion where
    toPath = const "/"

instance ToQuery UpdateApplicationVersion where
    toQuery UpdateApplicationVersion{..} = mconcat
        [ "ApplicationName" =? _uavApplicationName
        , "Description"     =? _uavDescription
        , "VersionLabel"    =? _uavVersionLabel
        ]

instance ToHeaders UpdateApplicationVersion

instance AWSRequest UpdateApplicationVersion where
    type Sv UpdateApplicationVersion = ElasticBeanstalk
    type Rs UpdateApplicationVersion = UpdateApplicationVersionResponse

    request  = post "UpdateApplicationVersion"
    response = xmlResponse

instance FromXML UpdateApplicationVersionResponse where
    parseXML = withElement "UpdateApplicationVersionResult" $ \x -> UpdateApplicationVersionResponse
        <$> x .@? "ApplicationVersion"
