{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

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
      CreateApplicationVersionMessage
    -- ** Request constructor
    , createApplicationVersionMessage
    -- ** Request lenses
    , cavmApplicationName
    , cavmAutoCreateApplication
    , cavmDescription
    , cavmSourceBundle
    , cavmVersionLabel

    -- * Response
    , ApplicationVersionDescriptionMessage
    -- ** Response constructor
    , applicationVersionDescriptionMessage
    -- ** Response lenses
    , avdmApplicationVersion
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ElasticBeanstalk.Types

data CreateApplicationVersionMessage = CreateApplicationVersionMessage
    { _cavmApplicationName       :: Text
    , _cavmAutoCreateApplication :: Maybe Bool
    , _cavmDescription           :: Maybe Text
    , _cavmSourceBundle          :: Maybe S3Location
    , _cavmVersionLabel          :: Text
    } (Eq, Show, Generic)

-- | 'CreateApplicationVersionMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cavmApplicationName' @::@ 'Text'
--
-- * 'cavmAutoCreateApplication' @::@ 'Maybe' 'Bool'
--
-- * 'cavmDescription' @::@ 'Maybe' 'Text'
--
-- * 'cavmSourceBundle' @::@ 'Maybe' 'S3Location'
--
-- * 'cavmVersionLabel' @::@ 'Text'
--
createApplicationVersionMessage :: Text -- ^ 'cavmApplicationName'
                                -> Text -- ^ 'cavmVersionLabel'
                                -> CreateApplicationVersionMessage
createApplicationVersionMessage p1 p2 = CreateApplicationVersionMessage
    { _cavmApplicationName       = p1
    , _cavmVersionLabel          = p2
    , _cavmDescription           = Nothing
    , _cavmSourceBundle          = Nothing
    , _cavmAutoCreateApplication = Nothing
    }

-- | The name of the application. If no application is found with this name,
-- and AutoCreateApplication is false, returns an InvalidParameterValue
-- error.
cavmApplicationName :: Lens' CreateApplicationVersionMessage Text
cavmApplicationName =
    lens _cavmApplicationName (\s a -> s { _cavmApplicationName = a })

-- | Determines how the system behaves if the specified application for this
-- version does not already exist: true: Automatically creates the specified
-- application for this version if it does not already exist. false: Returns
-- an InvalidParameterValue if the specified application for this version
-- does not already exist. true : Automatically creates the specified
-- application for this release if it does not already exist. false : Throws
-- an InvalidParameterValue if the specified application for this release
-- does not already exist. Default: false Valid Values: true | false.
cavmAutoCreateApplication :: Lens' CreateApplicationVersionMessage (Maybe Bool)
cavmAutoCreateApplication =
    lens _cavmAutoCreateApplication
        (\s a -> s { _cavmAutoCreateApplication = a })

-- | Describes this version.
cavmDescription :: Lens' CreateApplicationVersionMessage (Maybe Text)
cavmDescription = lens _cavmDescription (\s a -> s { _cavmDescription = a })

-- | The Amazon S3 bucket and key that identify the location of the source
-- bundle for this version. If data found at the Amazon S3 location exceeds
-- the maximum allowed source bundle size, AWS Elastic Beanstalk returns an
-- InvalidParameterValue error. The maximum size allowed is 512 MB. Default:
-- If not specified, AWS Elastic Beanstalk uses a sample application. If
-- only partially specified (for example, a bucket is provided but not the
-- key) or if no data is found at the Amazon S3 location, AWS Elastic
-- Beanstalk returns an InvalidParameterCombination error.
cavmSourceBundle :: Lens' CreateApplicationVersionMessage (Maybe S3Location)
cavmSourceBundle = lens _cavmSourceBundle (\s a -> s { _cavmSourceBundle = a })

-- | A label identifying this version. Constraint: Must be unique per
-- application. If an application version already exists with this label for
-- the specified application, AWS Elastic Beanstalk returns an
-- InvalidParameterValue error.
cavmVersionLabel :: Lens' CreateApplicationVersionMessage Text
cavmVersionLabel = lens _cavmVersionLabel (\s a -> s { _cavmVersionLabel = a })
instance ToQuery CreateApplicationVersionMessage

instance ToPath CreateApplicationVersionMessage where
    toPath = const "/"

instance AWSRequest CreateApplicationVersionMessage where
    type Sv CreateApplicationVersionMessage = ElasticBeanstalk
    type Rs CreateApplicationVersionMessage = ApplicationVersionDescriptionMessage

    request  = post "CreateApplicationVersion"
    response = xmlResponse $ const decodeCursor
