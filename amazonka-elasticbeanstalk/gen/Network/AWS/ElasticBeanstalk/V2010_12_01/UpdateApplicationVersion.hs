{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElasticBeanstalk.V2010_12_01.UpdateApplicationVersion
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Updates the specified application version to have the specified properties.
-- If a property (for example, description) is not provided, the value remains
-- unchanged. To clear properties, specify an empty string.
-- https://elasticbeanstalk.us-east-1.amazon.com/?ApplicationName=SampleApp
-- &VersionLabel=New%20Version &Description=New%20Release%20Description
-- &Operation=UpdateApplicationVersion &AuthParams awsemr sample.war New
-- Version New Release Description SampleApp 2010-11-17T19:26:20.699Z
-- 2010-11-17T20:48:16.632Z 00b10aa1-f28c-11df-8a78-9f77047e0d0c.
module Network.AWS.ElasticBeanstalk.V2010_12_01.UpdateApplicationVersion
    (
    -- * Request
      UpdateApplicationVersion
    -- ** Request constructor
    , mkUpdateApplicationVersionMessage
    -- ** Request lenses
    , uavmApplicationName
    , uavmVersionLabel
    , uavmDescription

    -- * Response
    , UpdateApplicationVersionResponse
    -- ** Response lenses
    , avdoApplicationVersion
    ) where

import Network.AWS.Request.Query
import Network.AWS.ElasticBeanstalk.V2010_12_01.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'UpdateApplicationVersion' request.
mkUpdateApplicationVersionMessage :: Text -- ^ 'uavmApplicationName'
                                  -> Text -- ^ 'uavmVersionLabel'
                                  -> UpdateApplicationVersion
mkUpdateApplicationVersionMessage p1 p2 = UpdateApplicationVersion
    { _uavmApplicationName = p1
    , _uavmVersionLabel = p2
    , _uavmDescription = Nothing
    }
{-# INLINE mkUpdateApplicationVersionMessage #-}

data UpdateApplicationVersion = UpdateApplicationVersion
    { _uavmApplicationName :: Text
      -- ^ The name of the application associated with this version. If no
      -- application is found with this name, UpdateApplication returns an
      -- InvalidParameterValue error.
    , _uavmVersionLabel :: Text
      -- ^ The name of the version to update. If no application version is
      -- found with this label, UpdateApplication returns an
      -- InvalidParameterValue error.
    , _uavmDescription :: Maybe Text
      -- ^ A new description for this release.
    } deriving (Show, Generic)

-- | The name of the application associated with this version. If no application
-- is found with this name, UpdateApplication returns an InvalidParameterValue
-- error.
uavmApplicationName :: Lens' UpdateApplicationVersion (Text)
uavmApplicationName = lens _uavmApplicationName (\s a -> s { _uavmApplicationName = a })
{-# INLINE uavmApplicationName #-}

-- | The name of the version to update. If no application version is found with
-- this label, UpdateApplication returns an InvalidParameterValue error.
uavmVersionLabel :: Lens' UpdateApplicationVersion (Text)
uavmVersionLabel = lens _uavmVersionLabel (\s a -> s { _uavmVersionLabel = a })
{-# INLINE uavmVersionLabel #-}

-- | A new description for this release.
uavmDescription :: Lens' UpdateApplicationVersion (Maybe Text)
uavmDescription = lens _uavmDescription (\s a -> s { _uavmDescription = a })
{-# INLINE uavmDescription #-}

instance ToQuery UpdateApplicationVersion where
    toQuery = genericQuery def

newtype UpdateApplicationVersionResponse = UpdateApplicationVersionResponse
    { _avdoApplicationVersion :: Maybe ApplicationVersionDescription
      -- ^ The ApplicationVersionDescription of the application version.
    } deriving (Show, Generic)

-- | The ApplicationVersionDescription of the application version.
avdoApplicationVersion :: Lens' UpdateApplicationVersionResponse (Maybe ApplicationVersionDescription)
avdoApplicationVersion = lens _avdoApplicationVersion (\s a -> s { _avdoApplicationVersion = a })
{-# INLINE avdoApplicationVersion #-}

instance FromXML UpdateApplicationVersionResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest UpdateApplicationVersion where
    type Sv UpdateApplicationVersion = ElasticBeanstalk
    type Rs UpdateApplicationVersion = UpdateApplicationVersionResponse

    request = post "UpdateApplicationVersion"
    response _ = xmlResponse
