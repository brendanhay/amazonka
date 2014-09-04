{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElasticBeanstalk.V2010_12_01.CreateApplicationVersion
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
module Network.AWS.ElasticBeanstalk.V2010_12_01.CreateApplicationVersion
    (
    -- * Request
      CreateApplicationVersion
    -- ** Request constructor
    , mkCreateApplicationVersionMessage
    -- ** Request lenses
    , cavmApplicationName
    , cavmVersionLabel
    , cavmDescription
    , cavmSourceBundle
    , cavmAutoCreateApplication

    -- * Response
    , CreateApplicationVersionResponse
    -- ** Response lenses
    , avdmApplicationVersion
    ) where

import Network.AWS.Request.Query
import Network.AWS.ElasticBeanstalk.V2010_12_01.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateApplicationVersion' request.
mkCreateApplicationVersionMessage :: Text -- ^ 'cavmApplicationName'
                                  -> Text -- ^ 'cavmVersionLabel'
                                  -> CreateApplicationVersion
mkCreateApplicationVersionMessage p1 p2 = CreateApplicationVersion
    { _cavmApplicationName = p1
    , _cavmVersionLabel = p2
    , _cavmDescription = Nothing
    , _cavmSourceBundle = Nothing
    , _cavmAutoCreateApplication = Nothing
    }
{-# INLINE mkCreateApplicationVersionMessage #-}

data CreateApplicationVersion = CreateApplicationVersion
    { _cavmApplicationName :: Text
      -- ^ The name of the application. If no application is found with this
      -- name, and AutoCreateApplication is false, returns an
      -- InvalidParameterValue error.
    , _cavmVersionLabel :: Text
      -- ^ A label identifying this version. Constraint: Must be unique per
      -- application. If an application version already exists with this
      -- label for the specified application, AWS Elastic Beanstalk
      -- returns an InvalidParameterValue error.
    , _cavmDescription :: Maybe Text
      -- ^ Describes this version.
    , _cavmSourceBundle :: Maybe S3Location
      -- ^ The Amazon S3 bucket and key that identify the location of the
      -- source bundle for this version. If data found at the Amazon S3
      -- location exceeds the maximum allowed source bundle size, AWS
      -- Elastic Beanstalk returns an InvalidParameterValue error. The
      -- maximum size allowed is 512 MB. Default: If not specified, AWS
      -- Elastic Beanstalk uses a sample application. If only partially
      -- specified (for example, a bucket is provided but not the key) or
      -- if no data is found at the Amazon S3 location, AWS Elastic
      -- Beanstalk returns an InvalidParameterCombination error.
    , _cavmAutoCreateApplication :: Maybe Bool
      -- ^ Determines how the system behaves if the specified application
      -- for this version does not already exist: true: Automatically
      -- creates the specified application for this version if it does not
      -- already exist. false: Returns an InvalidParameterValue if the
      -- specified application for this version does not already exist.
      -- true : Automatically creates the specified application for this
      -- release if it does not already exist. false : Throws an
      -- InvalidParameterValue if the specified application for this
      -- release does not already exist. Default: false Valid Values: true
      -- | false.
    } deriving (Show, Generic)

-- | The name of the application. If no application is found with this name, and
-- AutoCreateApplication is false, returns an InvalidParameterValue error.
cavmApplicationName :: Lens' CreateApplicationVersion (Text)
cavmApplicationName = lens _cavmApplicationName (\s a -> s { _cavmApplicationName = a })
{-# INLINE cavmApplicationName #-}

-- | A label identifying this version. Constraint: Must be unique per
-- application. If an application version already exists with this label for
-- the specified application, AWS Elastic Beanstalk returns an
-- InvalidParameterValue error.
cavmVersionLabel :: Lens' CreateApplicationVersion (Text)
cavmVersionLabel = lens _cavmVersionLabel (\s a -> s { _cavmVersionLabel = a })
{-# INLINE cavmVersionLabel #-}

-- | Describes this version.
cavmDescription :: Lens' CreateApplicationVersion (Maybe Text)
cavmDescription = lens _cavmDescription (\s a -> s { _cavmDescription = a })
{-# INLINE cavmDescription #-}

-- | The Amazon S3 bucket and key that identify the location of the source
-- bundle for this version. If data found at the Amazon S3 location exceeds
-- the maximum allowed source bundle size, AWS Elastic Beanstalk returns an
-- InvalidParameterValue error. The maximum size allowed is 512 MB. Default:
-- If not specified, AWS Elastic Beanstalk uses a sample application. If only
-- partially specified (for example, a bucket is provided but not the key) or
-- if no data is found at the Amazon S3 location, AWS Elastic Beanstalk
-- returns an InvalidParameterCombination error.
cavmSourceBundle :: Lens' CreateApplicationVersion (Maybe S3Location)
cavmSourceBundle = lens _cavmSourceBundle (\s a -> s { _cavmSourceBundle = a })
{-# INLINE cavmSourceBundle #-}

-- | Determines how the system behaves if the specified application for this
-- version does not already exist: true: Automatically creates the specified
-- application for this version if it does not already exist. false: Returns
-- an InvalidParameterValue if the specified application for this version does
-- not already exist. true : Automatically creates the specified application
-- for this release if it does not already exist. false : Throws an
-- InvalidParameterValue if the specified application for this release does
-- not already exist. Default: false Valid Values: true | false.
cavmAutoCreateApplication :: Lens' CreateApplicationVersion (Maybe Bool)
cavmAutoCreateApplication = lens _cavmAutoCreateApplication (\s a -> s { _cavmAutoCreateApplication = a })
{-# INLINE cavmAutoCreateApplication #-}

instance ToQuery CreateApplicationVersion where
    toQuery = genericQuery def

newtype CreateApplicationVersionResponse = CreateApplicationVersionResponse
    { _avdmApplicationVersion :: Maybe ApplicationVersionDescription
      -- ^ The ApplicationVersionDescription of the application version.
    } deriving (Show, Generic)

-- | The ApplicationVersionDescription of the application version.
avdmApplicationVersion :: Lens' CreateApplicationVersionResponse (Maybe ApplicationVersionDescription)
avdmApplicationVersion = lens _avdmApplicationVersion (\s a -> s { _avdmApplicationVersion = a })
{-# INLINE avdmApplicationVersion #-}

instance FromXML CreateApplicationVersionResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest CreateApplicationVersion where
    type Sv CreateApplicationVersion = ElasticBeanstalk
    type Rs CreateApplicationVersion = CreateApplicationVersionResponse

    request = post "CreateApplicationVersion"
    response _ = xmlResponse
