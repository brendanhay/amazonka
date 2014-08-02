{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

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
module Network.AWS.ElasticBeanstalk.V2010_12_01.UpdateApplicationVersion where

import Control.Lens
import Network.AWS.Request.Query
import Network.AWS.ElasticBeanstalk.V2010_12_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'UpdateApplicationVersion' request.
updateApplicationVersion :: Text -- ^ '_uavmApplicationName'
                         -> Text -- ^ '_uavmVersionLabel'
                         -> UpdateApplicationVersion
updateApplicationVersion p1 p2 = UpdateApplicationVersion
    { _uavmApplicationName = p1
    , _uavmVersionLabel = p2
    , _uavmDescription = Nothing
    }

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
    } deriving (Generic)

makeLenses ''UpdateApplicationVersion

instance ToQuery UpdateApplicationVersion where
    toQuery = genericToQuery def

data UpdateApplicationVersionResponse = UpdateApplicationVersionResponse
    { _avdnApplicationVersion :: Maybe ApplicationVersionDescription
      -- ^ The ApplicationVersionDescription of the application version.
    } deriving (Generic)

makeLenses ''UpdateApplicationVersionResponse

instance FromXML UpdateApplicationVersionResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest UpdateApplicationVersion where
    type Sv UpdateApplicationVersion = ElasticBeanstalk
    type Rs UpdateApplicationVersion = UpdateApplicationVersionResponse

    request = post "UpdateApplicationVersion"
    response _ = xmlResponse
