{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElasticBeanstalk.UpdateApplication
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
module Network.AWS.ElasticBeanstalk.UpdateApplication
    (
    -- * Request
      UpdateApplication
    -- ** Request constructor
    , mkUpdateApplication
    -- ** Request lenses
    , uaApplicationName
    , uaDescription

    -- * Response
    , UpdateApplicationResponse
    -- ** Response constructor
    , mkUpdateApplicationResponse
    -- ** Response lenses
    , uarApplication
    ) where

import Network.AWS.Request.Query
import Network.AWS.ElasticBeanstalk.Types
import Network.AWS.Prelude

-- | This documentation target is not reported in the API reference.
data UpdateApplication = UpdateApplication
    { _uaApplicationName :: Text
    , _uaDescription :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'UpdateApplication' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ApplicationName ::@ @Text@
--
-- * @Description ::@ @Maybe Text@
--
mkUpdateApplication :: Text -- ^ 'uaApplicationName'
                    -> UpdateApplication
mkUpdateApplication p1 = UpdateApplication
    { _uaApplicationName = p1
    , _uaDescription = Nothing
    }

-- | The name of the application to update. If no such application is found,
-- UpdateApplication returns an InvalidParameterValue error.
uaApplicationName :: Lens' UpdateApplication Text
uaApplicationName =
    lens _uaApplicationName (\s a -> s { _uaApplicationName = a })

-- | A new description for the application. Default: If not specified, AWS
-- Elastic Beanstalk does not update the description.
uaDescription :: Lens' UpdateApplication (Maybe Text)
uaDescription = lens _uaDescription (\s a -> s { _uaDescription = a })

instance ToQuery UpdateApplication where
    toQuery = genericQuery def

-- | Result message containing a single description of an application.
newtype UpdateApplicationResponse = UpdateApplicationResponse
    { _uarApplication :: Maybe ApplicationDescription
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'UpdateApplicationResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Application ::@ @Maybe ApplicationDescription@
--
mkUpdateApplicationResponse :: UpdateApplicationResponse
mkUpdateApplicationResponse = UpdateApplicationResponse
    { _uarApplication = Nothing
    }

-- | The ApplicationDescription of the application.
uarApplication :: Lens' UpdateApplicationResponse (Maybe ApplicationDescription)
uarApplication = lens _uarApplication (\s a -> s { _uarApplication = a })

instance FromXML UpdateApplicationResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest UpdateApplication where
    type Sv UpdateApplication = ElasticBeanstalk
    type Rs UpdateApplication = UpdateApplicationResponse

    request = post "UpdateApplication"
    response _ = xmlResponse
