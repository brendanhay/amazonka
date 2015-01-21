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

-- Module      : Network.AWS.CodeDeploy.GetApplication
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Gets information about an application.
--
-- <http://docs.aws.amazon.com/codedeploy/latest/APIReference/API_GetApplication.html>
module Network.AWS.CodeDeploy.GetApplication
    (
    -- * Request
      GetApplication
    -- ** Request constructor
    , getApplication
    -- ** Request lenses
    , gaApplicationName

    -- * Response
    , GetApplicationResponse
    -- ** Response constructor
    , getApplicationResponse
    -- ** Response lenses
    , garApplication
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.CodeDeploy.Types
import qualified GHC.Exts

newtype GetApplication = GetApplication
    { _gaApplicationName :: Text
    } deriving (Eq, Ord, Read, Show, Monoid, IsString)

-- | 'GetApplication' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gaApplicationName' @::@ 'Text'
--
getApplication :: Text -- ^ 'gaApplicationName'
               -> GetApplication
getApplication p1 = GetApplication
    { _gaApplicationName = p1
    }

-- | The name of an existing AWS CodeDeploy application within the AWS user
-- account.
gaApplicationName :: Lens' GetApplication Text
gaApplicationName =
    lens _gaApplicationName (\s a -> s { _gaApplicationName = a })

newtype GetApplicationResponse = GetApplicationResponse
    { _garApplication :: Maybe ApplicationInfo
    } deriving (Eq, Read, Show)

-- | 'GetApplicationResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'garApplication' @::@ 'Maybe' 'ApplicationInfo'
--
getApplicationResponse :: GetApplicationResponse
getApplicationResponse = GetApplicationResponse
    { _garApplication = Nothing
    }

-- | Information about the application.
garApplication :: Lens' GetApplicationResponse (Maybe ApplicationInfo)
garApplication = lens _garApplication (\s a -> s { _garApplication = a })

instance ToPath GetApplication where
    toPath = const "/"

instance ToQuery GetApplication where
    toQuery = const mempty

instance ToHeaders GetApplication

instance ToJSON GetApplication where
    toJSON GetApplication{..} = object
        [ "applicationName" .= _gaApplicationName
        ]

instance AWSRequest GetApplication where
    type Sv GetApplication = CodeDeploy
    type Rs GetApplication = GetApplicationResponse

    request  = post "GetApplication"
    response = jsonResponse

instance FromJSON GetApplicationResponse where
    parseJSON = withObject "GetApplicationResponse" $ \o -> GetApplicationResponse
        <$> o .:? "application"
