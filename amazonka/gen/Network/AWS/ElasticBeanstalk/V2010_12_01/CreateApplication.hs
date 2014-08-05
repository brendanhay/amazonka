{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

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
module Network.AWS.ElasticBeanstalk.V2010_12_01.CreateApplication where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.Query
import Network.AWS.ElasticBeanstalk.V2010_12_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'CreateApplication' request.
createApplication :: Text -- ^ '_camApplicationName'
                  -> CreateApplication
createApplication p1 = CreateApplication
    { _camApplicationName = p1
    , _camDescription = Nothing
    }

data CreateApplication = CreateApplication
    { _camApplicationName :: Text
      -- ^ The name of the application. Constraint: This name must be unique
      -- within your account. If the specified name already exists, the
      -- action returns an InvalidParameterValue error.
    , _camDescription :: Maybe Text
      -- ^ Describes the application.
    } deriving (Show, Generic)

makeLenses ''CreateApplication

instance ToQuery CreateApplication where
    toQuery = genericToQuery def

data CreateApplicationResponse = CreateApplicationResponse
    { _adoApplication :: Maybe ApplicationDescription
      -- ^ The ApplicationDescription of the application.
    } deriving (Show, Generic)

makeLenses ''CreateApplicationResponse

instance AWSRequest CreateApplication where
    type Sv CreateApplication = ElasticBeanstalk
    type Rs CreateApplication = CreateApplicationResponse

    request = post "CreateApplication"
    response _ = cursorResponse $ \hs xml ->
        pure CreateApplicationResponse
            <*> xml %|? "ApplicationDescription"
