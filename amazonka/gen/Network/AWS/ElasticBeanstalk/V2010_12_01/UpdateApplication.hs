{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElasticBeanstalk.V2010_12_01.UpdateApplication
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Updates the specified application to have the specified properties. If a
-- property (for example, description) is not provided, the value remains
-- unchanged. To clear these properties, specify an empty string.
-- https://elasticbeanstalk.us-east-1.amazon.com/?ApplicationName=SampleApp
-- &Description=Another%20Description &Operation=UpdateApplication &AuthParams
-- New Version Another Description SampleApp 2010-11-17T19:26:20.410Z
-- 2010-11-17T20:42:54.611Z Default 40be666b-f28b-11df-8a78-9f77047e0d0c.
module Network.AWS.ElasticBeanstalk.V2010_12_01.UpdateApplication where

import           Control.Applicative
import           Data.ByteString      (ByteString)
import           Data.Default
import           Data.HashMap.Strict  (HashMap)
import           Data.Monoid
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           GHC.Generics
import           Network.AWS.Data
import           Network.AWS.Response
import           Network.AWS.Types    hiding (Error, Endpoint, Region)
import           Network.AWS.Request.Query
import           Network.AWS.ElasticBeanstalk.V2010_12_01.Types
import           Network.HTTP.Client  (RequestBody, Response)
import           Prelude              hiding (head)

-- | Minimum specification for a 'UpdateApplication' request.
updateApplication :: Text -- ^ '_uamApplicationName'
                  -> UpdateApplication
updateApplication p1 = UpdateApplication
    { _uamApplicationName = p1
    , _uamDescription = Nothing
    }

data UpdateApplication = UpdateApplication
    { _uamApplicationName :: Text
      -- ^ The name of the application to update. If no such application is
      -- found, UpdateApplication returns an InvalidParameterValue error.
    , _uamDescription :: Maybe Text
      -- ^ A new description for the application. Default: If not specified,
      -- AWS Elastic Beanstalk does not update the description.
    } deriving (Generic)

instance ToQuery UpdateApplication where
    toQuery = genericToQuery def

instance AWSRequest UpdateApplication where
    type Sv UpdateApplication = ElasticBeanstalk
    type Rs UpdateApplication = UpdateApplicationResponse

    request = post "UpdateApplication"
    response _ = xmlResponse

data UpdateApplicationResponse = UpdateApplicationResponse
    { _adnApplication :: Maybe ApplicationDescription
      -- ^ The ApplicationDescription of the application.
    } deriving (Generic)

instance FromXML UpdateApplicationResponse where
    fromXMLOptions = xmlOptions
