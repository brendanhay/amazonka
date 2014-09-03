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
    , createApplicationVersion
    -- ** Request lenses
    , cavmApplicationName
    , cavmVersionLabel
    , cavmAutoCreateApplication
    , cavmDescription
    , cavmSourceBundle

    -- * Response
    , CreateApplicationVersionResponse
    -- ** Response lenses
    , avdmApplicationVersion
    ) where

import Network.AWS.Request.Query
import Network.AWS.ElasticBeanstalk.V2010_12_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'CreateApplicationVersion' request.
createApplicationVersion :: Text -- ^ 'cavmApplicationName'
                         -> Text -- ^ 'cavmVersionLabel'
                         -> CreateApplicationVersion
createApplicationVersion p1 p2 = CreateApplicationVersion
    { _cavmApplicationName = p1
    , _cavmVersionLabel = p2
    , _cavmAutoCreateApplication = Nothing
    , _cavmDescription = Nothing
    , _cavmSourceBundle = Nothing
    }

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
    } deriving (Show, Generic)

-- | The name of the application. If no application is found with this name, and
-- AutoCreateApplication is false, returns an InvalidParameterValue error.
cavmApplicationName
    :: Functor f
    => (Text
    -> f (Text))
    -> CreateApplicationVersion
    -> f CreateApplicationVersion
cavmApplicationName f x =
    (\y -> x { _cavmApplicationName = y })
       <$> f (_cavmApplicationName x)
{-# INLINE cavmApplicationName #-}

-- | A label identifying this version. Constraint: Must be unique per
-- application. If an application version already exists with this label for
-- the specified application, AWS Elastic Beanstalk returns an
-- InvalidParameterValue error.
cavmVersionLabel
    :: Functor f
    => (Text
    -> f (Text))
    -> CreateApplicationVersion
    -> f CreateApplicationVersion
cavmVersionLabel f x =
    (\y -> x { _cavmVersionLabel = y })
       <$> f (_cavmVersionLabel x)
{-# INLINE cavmVersionLabel #-}

-- | Determines how the system behaves if the specified application for this
-- version does not already exist: true: Automatically creates the specified
-- application for this version if it does not already exist. false: Returns
-- an InvalidParameterValue if the specified application for this version does
-- not already exist. true : Automatically creates the specified application
-- for this release if it does not already exist. false : Throws an
-- InvalidParameterValue if the specified application for this release does
-- not already exist. Default: false Valid Values: true | false.
cavmAutoCreateApplication
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> CreateApplicationVersion
    -> f CreateApplicationVersion
cavmAutoCreateApplication f x =
    (\y -> x { _cavmAutoCreateApplication = y })
       <$> f (_cavmAutoCreateApplication x)
{-# INLINE cavmAutoCreateApplication #-}

-- | Describes this version.
cavmDescription
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CreateApplicationVersion
    -> f CreateApplicationVersion
cavmDescription f x =
    (\y -> x { _cavmDescription = y })
       <$> f (_cavmDescription x)
{-# INLINE cavmDescription #-}

-- | The Amazon S3 bucket and key that identify the location of the source
-- bundle for this version. If data found at the Amazon S3 location exceeds
-- the maximum allowed source bundle size, AWS Elastic Beanstalk returns an
-- InvalidParameterValue error. The maximum size allowed is 512 MB. Default:
-- If not specified, AWS Elastic Beanstalk uses a sample application. If only
-- partially specified (for example, a bucket is provided but not the key) or
-- if no data is found at the Amazon S3 location, AWS Elastic Beanstalk
-- returns an InvalidParameterCombination error.
cavmSourceBundle
    :: Functor f
    => (Maybe S3Location
    -> f (Maybe S3Location))
    -> CreateApplicationVersion
    -> f CreateApplicationVersion
cavmSourceBundle f x =
    (\y -> x { _cavmSourceBundle = y })
       <$> f (_cavmSourceBundle x)
{-# INLINE cavmSourceBundle #-}

instance ToQuery CreateApplicationVersion where
    toQuery = genericQuery def

data CreateApplicationVersionResponse = CreateApplicationVersionResponse
    { _avdmApplicationVersion :: Maybe ApplicationVersionDescription
      -- ^ The ApplicationVersionDescription of the application version.
    } deriving (Show, Generic)

-- | The ApplicationVersionDescription of the application version.
avdmApplicationVersion
    :: Functor f
    => (Maybe ApplicationVersionDescription
    -> f (Maybe ApplicationVersionDescription))
    -> CreateApplicationVersionResponse
    -> f CreateApplicationVersionResponse
avdmApplicationVersion f x =
    (\y -> x { _avdmApplicationVersion = y })
       <$> f (_avdmApplicationVersion x)
{-# INLINE avdmApplicationVersion #-}

instance FromXML CreateApplicationVersionResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest CreateApplicationVersion where
    type Sv CreateApplicationVersion = ElasticBeanstalk
    type Rs CreateApplicationVersion = CreateApplicationVersionResponse

    request = post "CreateApplicationVersion"
    response _ = xmlResponse
