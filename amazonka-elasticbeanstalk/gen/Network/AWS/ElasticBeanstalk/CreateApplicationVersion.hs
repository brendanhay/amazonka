{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
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

-- | Creates an application version for the specified application.
module Network.AWS.ElasticBeanstalk.CreateApplicationVersion
    (
    -- * Request
      CreateApplicationVersion
    -- ** Request constructor
    , createApplicationVersion
    -- ** Request lenses
    , cavApplicationName
    , cavAutoCreateApplication
    , cavDescription
    , cavSourceBundle
    , cavVersionLabel

    -- * Response
    , CreateApplicationVersionResponse
    -- ** Response constructor
    , createApplicationVersionResponse
    -- ** Response lenses
    , cavrApplicationVersion
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ElasticBeanstalk.Types
import qualified GHC.Exts

data CreateApplicationVersion = CreateApplicationVersion
    { _cavApplicationName       :: Text
    , _cavAutoCreateApplication :: Maybe Bool
    , _cavDescription           :: Maybe Text
    , _cavSourceBundle          :: Maybe S3Location
    , _cavVersionLabel          :: Text
    } deriving (Eq, Show, Generic)

-- | 'CreateApplicationVersion' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cavApplicationName' @::@ 'Text'
--
-- * 'cavAutoCreateApplication' @::@ 'Maybe' 'Bool'
--
-- * 'cavDescription' @::@ 'Maybe' 'Text'
--
-- * 'cavSourceBundle' @::@ 'Maybe' 'S3Location'
--
-- * 'cavVersionLabel' @::@ 'Text'
--
createApplicationVersion :: Text -- ^ 'cavApplicationName'
                         -> Text -- ^ 'cavVersionLabel'
                         -> CreateApplicationVersion
createApplicationVersion p1 p2 = CreateApplicationVersion
    { _cavApplicationName       = p1
    , _cavVersionLabel          = p2
    , _cavDescription           = Nothing
    , _cavSourceBundle          = Nothing
    , _cavAutoCreateApplication = Nothing
    }

-- | The name of the application. If no application is found with this name,
-- and AutoCreateApplication is false, returns an InvalidParameterValue
-- error.
cavApplicationName :: Lens' CreateApplicationVersion Text
cavApplicationName =
    lens _cavApplicationName (\s a -> s { _cavApplicationName = a })

-- | Determines how the system behaves if the specified application for this
-- version does not already exist: true: Automatically creates the specified
-- application for this version if it does not already exist. false: Returns
-- an InvalidParameterValue if the specified application for this version
-- does not already exist. true : Automatically creates the specified
-- application for this release if it does not already exist. false : Throws
-- an InvalidParameterValue if the specified application for this release
-- does not already exist. Default: false Valid Values: true | false.
cavAutoCreateApplication :: Lens' CreateApplicationVersion (Maybe Bool)
cavAutoCreateApplication =
    lens _cavAutoCreateApplication
        (\s a -> s { _cavAutoCreateApplication = a })

-- | Describes this version.
cavDescription :: Lens' CreateApplicationVersion (Maybe Text)
cavDescription = lens _cavDescription (\s a -> s { _cavDescription = a })

-- | The Amazon S3 bucket and key that identify the location of the source
-- bundle for this version. If data found at the Amazon S3 location exceeds
-- the maximum allowed source bundle size, AWS Elastic Beanstalk returns an
-- InvalidParameterValue error. The maximum size allowed is 512 MB. Default:
-- If not specified, AWS Elastic Beanstalk uses a sample application. If
-- only partially specified (for example, a bucket is provided but not the
-- key) or if no data is found at the Amazon S3 location, AWS Elastic
-- Beanstalk returns an InvalidParameterCombination error.
cavSourceBundle :: Lens' CreateApplicationVersion (Maybe S3Location)
cavSourceBundle = lens _cavSourceBundle (\s a -> s { _cavSourceBundle = a })

-- | A label identifying this version. Constraint: Must be unique per
-- application. If an application version already exists with this label for
-- the specified application, AWS Elastic Beanstalk returns an
-- InvalidParameterValue error.
cavVersionLabel :: Lens' CreateApplicationVersion Text
cavVersionLabel = lens _cavVersionLabel (\s a -> s { _cavVersionLabel = a })

newtype CreateApplicationVersionResponse = CreateApplicationVersionResponse
    { _cavrApplicationVersion :: Maybe ApplicationVersionDescription
    } deriving (Eq, Show, Generic)

-- | 'CreateApplicationVersionResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cavrApplicationVersion' @::@ 'Maybe' 'ApplicationVersionDescription'
--
createApplicationVersionResponse :: CreateApplicationVersionResponse
createApplicationVersionResponse = CreateApplicationVersionResponse
    { _cavrApplicationVersion = Nothing
    }

-- | The ApplicationVersionDescription of the application version.
cavrApplicationVersion :: Lens' CreateApplicationVersionResponse (Maybe ApplicationVersionDescription)
cavrApplicationVersion =
    lens _cavrApplicationVersion (\s a -> s { _cavrApplicationVersion = a })

instance AWSRequest CreateApplicationVersion where
    type Sv CreateApplicationVersion = ElasticBeanstalk
    type Rs CreateApplicationVersion = CreateApplicationVersionResponse

    request  = post "CreateApplicationVersion"
    response = xmlResponse

instance FromXML CreateApplicationVersionResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "CreateApplicationVersionResponse"

instance ToPath CreateApplicationVersion where
    toPath = const "/"

instance ToHeaders CreateApplicationVersion

instance ToQuery CreateApplicationVersion
