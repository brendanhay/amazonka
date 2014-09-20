{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElasticBeanstalk.CreateApplicationVersion
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates an application version for the specified application. Once you
-- create an application version with a specified Amazon S3 bucket and key
-- location, you cannot change that Amazon S3 location. If you change the
-- Amazon S3 location, you receive an exception when you attempt to launch an
-- environment from the application version.
-- https://elasticbeanstalk.us-east-1.amazon.com/?ApplicationName=SampleApp
-- &VersionLabel=Version1 &Description=description
-- &SourceBundle.S3Bucket=amazonaws.com &SourceBundle.S3Key=sample.war
-- &AutoCreateApplication=true &Operation=CreateApplicationVersion &AuthParams
-- amazonaws.com sample.war Version1 description SampleApp
-- 2010-11-17T03:21:59.161Z 2010-11-17T03:21:59.161Z
-- d653efef-f1f9-11df-8a78-9f77047e0d0c.
module Network.AWS.ElasticBeanstalk.CreateApplicationVersion
    (
    -- * Request
      CreateApplicationVersion
    -- ** Request constructor
    , createApplicationVersion
    -- ** Request lenses
    , cavApplicationName
    , cavVersionLabel
    , cavDescription
    , cavSourceBundle
    , cavAutoCreateApplication

    -- * Response
    , CreateApplicationVersionResponse
    -- ** Response constructor
    , createApplicationVersionResponse
    -- ** Response lenses
    , cavrApplicationVersion
    ) where

import Network.AWS.Request.Query
import Network.AWS.ElasticBeanstalk.Types
import Network.AWS.Prelude

-- | 
data CreateApplicationVersion = CreateApplicationVersion
    { _cavApplicationName :: Text
    , _cavVersionLabel :: Text
    , _cavDescription :: Maybe Text
    , _cavSourceBundle :: Maybe S3Location
    , _cavAutoCreateApplication :: Maybe Bool
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateApplicationVersion' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ApplicationName ::@ @Text@
--
-- * @VersionLabel ::@ @Text@
--
-- * @Description ::@ @Maybe Text@
--
-- * @SourceBundle ::@ @Maybe S3Location@
--
-- * @AutoCreateApplication ::@ @Maybe Bool@
--
createApplicationVersion :: Text -- ^ 'cavApplicationName'
                         -> Text -- ^ 'cavVersionLabel'
                         -> CreateApplicationVersion
createApplicationVersion p1 p2 = CreateApplicationVersion
    { _cavApplicationName = p1
    , _cavVersionLabel = p2
    , _cavDescription = Nothing
    , _cavSourceBundle = Nothing
    , _cavAutoCreateApplication = Nothing
    }

-- | The name of the application. If no application is found with this name, and
-- AutoCreateApplication is false, returns an InvalidParameterValue error.
cavApplicationName :: Lens' CreateApplicationVersion Text
cavApplicationName =
    lens _cavApplicationName (\s a -> s { _cavApplicationName = a })

-- | A label identifying this version. Constraint: Must be unique per
-- application. If an application version already exists with this label for
-- the specified application, AWS Elastic Beanstalk returns an
-- InvalidParameterValue error.
cavVersionLabel :: Lens' CreateApplicationVersion Text
cavVersionLabel = lens _cavVersionLabel (\s a -> s { _cavVersionLabel = a })

-- | Describes this version.
cavDescription :: Lens' CreateApplicationVersion (Maybe Text)
cavDescription = lens _cavDescription (\s a -> s { _cavDescription = a })

-- | The Amazon S3 bucket and key that identify the location of the source
-- bundle for this version. If data found at the Amazon S3 location exceeds
-- the maximum allowed source bundle size, AWS Elastic Beanstalk returns an
-- InvalidParameterValue error. The maximum size allowed is 512 MB. Default:
-- If not specified, AWS Elastic Beanstalk uses a sample application. If only
-- partially specified (for example, a bucket is provided but not the key) or
-- if no data is found at the Amazon S3 location, AWS Elastic Beanstalk
-- returns an InvalidParameterCombination error.
cavSourceBundle :: Lens' CreateApplicationVersion (Maybe S3Location)
cavSourceBundle = lens _cavSourceBundle (\s a -> s { _cavSourceBundle = a })

-- | Determines how the system behaves if the specified application for this
-- version does not already exist: true: Automatically creates the specified
-- application for this version if it does not already exist. false: Returns
-- an InvalidParameterValue if the specified application for this version does
-- not already exist. true : Automatically creates the specified application
-- for this release if it does not already exist. false : Throws an
-- InvalidParameterValue if the specified application for this release does
-- not already exist. Default: false Valid Values: true | false.
cavAutoCreateApplication :: Lens' CreateApplicationVersion (Maybe Bool)
cavAutoCreateApplication =
    lens _cavAutoCreateApplication
         (\s a -> s { _cavAutoCreateApplication = a })

instance ToQuery CreateApplicationVersion where
    toQuery = genericQuery def

-- | Result message wrapping a single description of an application version.
newtype CreateApplicationVersionResponse = CreateApplicationVersionResponse
    { _cavrApplicationVersion :: Maybe ApplicationVersionDescription
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateApplicationVersionResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ApplicationVersion ::@ @Maybe ApplicationVersionDescription@
--
createApplicationVersionResponse :: CreateApplicationVersionResponse
createApplicationVersionResponse = CreateApplicationVersionResponse
    { _cavrApplicationVersion = Nothing
    }

-- | The ApplicationVersionDescription of the application version.
cavrApplicationVersion :: Lens' CreateApplicationVersionResponse (Maybe ApplicationVersionDescription)
cavrApplicationVersion =
    lens _cavrApplicationVersion (\s a -> s { _cavrApplicationVersion = a })

instance FromXML CreateApplicationVersionResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest CreateApplicationVersion where
    type Sv CreateApplicationVersion = ElasticBeanstalk
    type Rs CreateApplicationVersion = CreateApplicationVersionResponse

    request = post "CreateApplicationVersion"
    response _ = xmlResponse
