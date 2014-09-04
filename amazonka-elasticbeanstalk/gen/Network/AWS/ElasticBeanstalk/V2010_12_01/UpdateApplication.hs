{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
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
module Network.AWS.ElasticBeanstalk.V2010_12_01.UpdateApplication
    (
    -- * Request
      UpdateApplication
    -- ** Request constructor
    , mkUpdateApplicationMessage
    -- ** Request lenses
    , uamApplicationName
    , uamDescription

    -- * Response
    , UpdateApplicationResponse
    -- ** Response lenses
    , adoApplication
    ) where

import Network.AWS.Request.Query
import Network.AWS.ElasticBeanstalk.V2010_12_01.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'UpdateApplication' request.
mkUpdateApplicationMessage :: Text -- ^ 'uamApplicationName'
                           -> UpdateApplication
mkUpdateApplicationMessage p1 = UpdateApplication
    { _uamApplicationName = p1
    , _uamDescription = Nothing
    }
{-# INLINE mkUpdateApplicationMessage #-}

data UpdateApplication = UpdateApplication
    { _uamApplicationName :: Text
      -- ^ The name of the application to update. If no such application is
      -- found, UpdateApplication returns an InvalidParameterValue error.
    , _uamDescription :: Maybe Text
      -- ^ A new description for the application. Default: If not specified,
      -- AWS Elastic Beanstalk does not update the description.
    } deriving (Show, Generic)

-- | The name of the application to update. If no such application is found,
-- UpdateApplication returns an InvalidParameterValue error.
uamApplicationName :: Lens' UpdateApplication (Text)
uamApplicationName = lens _uamApplicationName (\s a -> s { _uamApplicationName = a })
{-# INLINE uamApplicationName #-}

-- | A new description for the application. Default: If not specified, AWS
-- Elastic Beanstalk does not update the description.
uamDescription :: Lens' UpdateApplication (Maybe Text)
uamDescription = lens _uamDescription (\s a -> s { _uamDescription = a })
{-# INLINE uamDescription #-}

instance ToQuery UpdateApplication where
    toQuery = genericQuery def

newtype UpdateApplicationResponse = UpdateApplicationResponse
    { _adoApplication :: Maybe ApplicationDescription
      -- ^ The ApplicationDescription of the application.
    } deriving (Show, Generic)

-- | The ApplicationDescription of the application.
adoApplication :: Lens' UpdateApplicationResponse (Maybe ApplicationDescription)
adoApplication = lens _adoApplication (\s a -> s { _adoApplication = a })
{-# INLINE adoApplication #-}

instance FromXML UpdateApplicationResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest UpdateApplication where
    type Sv UpdateApplication = ElasticBeanstalk
    type Rs UpdateApplication = UpdateApplicationResponse

    request = post "UpdateApplication"
    response _ = xmlResponse
