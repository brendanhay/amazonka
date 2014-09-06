{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElasticBeanstalk.V2010_12_01.CreateApplication
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates an application that has one configuration template named default
-- and no application versions. The &lt;code&gt;default&lt;/code&gt;
-- configuration template is for a 32-bit version of the Amazon Linux
-- operating system running the Tomcat 6 application container.
-- &lt;/note&gt;">
-- https://elasticbeanstalk.us-east-1.amazon.com/?ApplicationName=SampleApp
-- &Description=Sample%20Description &Operation=CreateApplication &AuthParams
-- Sample Description SampleApp 2010-11-16T23:09:20.256Z
-- 2010-11-16T23:09:20.256Z Default 8b00e053-f1d6-11df-8a78-9f77047e0d0c.
module Network.AWS.ElasticBeanstalk.V2010_12_01.CreateApplication
    (
    -- * Request
      CreateApplication
    -- ** Request constructor
    , mkCreateApplication
    -- ** Request lenses
    , caApplicationName
    , caDescription

    -- * Response
    , CreateApplicationResponse
    -- ** Response lenses
    , carsApplication
    ) where

import Network.AWS.Request.Query
import Network.AWS.ElasticBeanstalk.V2010_12_01.Types
import Network.AWS.Prelude

-- | This documentation target is not reported in the API reference.
data CreateApplication = CreateApplication
    { _caApplicationName :: Text
    , _caDescription :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateApplication' request.
mkCreateApplication :: Text -- ^ 'caApplicationName'
                    -> CreateApplication
mkCreateApplication p1 = CreateApplication
    { _caApplicationName = p1
    , _caDescription = Nothing
    }
{-# INLINE mkCreateApplication #-}

-- | The name of the application. Constraint: This name must be unique within
-- your account. If the specified name already exists, the action returns an
-- InvalidParameterValue error.
caApplicationName :: Lens' CreateApplication Text
caApplicationName =
    lens _caApplicationName (\s a -> s { _caApplicationName = a })
{-# INLINE caApplicationName #-}

-- | Describes the application.
caDescription :: Lens' CreateApplication (Maybe Text)
caDescription = lens _caDescription (\s a -> s { _caDescription = a })
{-# INLINE caDescription #-}

instance ToQuery CreateApplication where
    toQuery = genericQuery def

-- | Result message containing a single description of an application.
newtype CreateApplicationResponse = CreateApplicationResponse
    { _carsApplication :: Maybe ApplicationDescription
    } deriving (Show, Generic)

-- | The ApplicationDescription of the application.
carsApplication :: Lens' CreateApplicationResponse (Maybe ApplicationDescription)
carsApplication = lens _carsApplication (\s a -> s { _carsApplication = a })
{-# INLINE carsApplication #-}

instance FromXML CreateApplicationResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest CreateApplication where
    type Sv CreateApplication = ElasticBeanstalk
    type Rs CreateApplication = CreateApplicationResponse

    request = post "CreateApplication"
    response _ = xmlResponse
