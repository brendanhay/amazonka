{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElasticBeanstalk.CreateApplication
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates an application that has one configuration template named @default@
-- and no application versions.
--
-- <http://docs.aws.amazon.com/elasticbeanstalk/latest/api/API_CreateApplication.html>
module Network.AWS.ElasticBeanstalk.CreateApplication
    (
    -- * Request
      CreateApplication
    -- ** Request constructor
    , createApplication
    -- ** Request lenses
    , caApplicationName
    , caDescription

    -- * Response
    , CreateApplicationResponse
    -- ** Response constructor
    , createApplicationResponse
    -- ** Response lenses
    , carApplication
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ElasticBeanstalk.Types
import qualified GHC.Exts

data CreateApplication = CreateApplication
    { _caApplicationName :: Text
    , _caDescription     :: Maybe Text
    } deriving (Eq, Ord, Show)

-- | 'CreateApplication' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'caApplicationName' @::@ 'Text'
--
-- * 'caDescription' @::@ 'Maybe' 'Text'
--
createApplication :: Text -- ^ 'caApplicationName'
                  -> CreateApplication
createApplication p1 = CreateApplication
    { _caApplicationName = p1
    , _caDescription     = Nothing
    }

-- | The name of the application. Constraint: This name must be unique within
-- your account. If the specified name already exists, the action returns an
-- @InvalidParameterValue@ error.
caApplicationName :: Lens' CreateApplication Text
caApplicationName =
    lens _caApplicationName (\s a -> s { _caApplicationName = a })

-- | Describes the application.
caDescription :: Lens' CreateApplication (Maybe Text)
caDescription = lens _caDescription (\s a -> s { _caDescription = a })

newtype CreateApplicationResponse = CreateApplicationResponse
    { _carApplication :: Maybe ApplicationDescription
    } deriving (Eq, Show)

-- | 'CreateApplicationResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'carApplication' @::@ 'Maybe' 'ApplicationDescription'
--
createApplicationResponse :: CreateApplicationResponse
createApplicationResponse = CreateApplicationResponse
    { _carApplication = Nothing
    }

-- | The ApplicationDescription> of the application.
carApplication :: Lens' CreateApplicationResponse (Maybe ApplicationDescription)
carApplication = lens _carApplication (\s a -> s { _carApplication = a })

instance ToPath CreateApplication where
    toPath = const "/"

instance ToQuery CreateApplication where
    toQuery CreateApplication{..} = mconcat
        [ "ApplicationName" =? _caApplicationName
        , "Description"     =? _caDescription
        ]

instance ToHeaders CreateApplication

instance AWSRequest CreateApplication where
    type Sv CreateApplication = ElasticBeanstalk
    type Rs CreateApplication = CreateApplicationResponse

    request  = post "CreateApplication"
    response = xmlResponse

instance FromXML CreateApplicationResponse where
    parseXML = withElement "CreateApplicationResult" $ \x -> CreateApplicationResponse
        <$> x .@? "Application"
