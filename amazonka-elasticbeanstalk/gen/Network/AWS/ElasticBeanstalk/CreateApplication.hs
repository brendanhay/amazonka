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

-- Module      : Network.AWS.ElasticBeanstalk.CreateApplication
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates an application that has one configuration template named default
-- and no application versions.
module Network.AWS.ElasticBeanstalk.CreateApplication
    (
    -- * Request
      CreateApplicationMessage
    -- ** Request constructor
    , createApplicationMessage
    -- ** Request lenses
    , camApplicationName
    , camDescription

    -- * Response
    , ApplicationDescriptionMessage
    -- ** Response constructor
    , applicationDescriptionMessage
    -- ** Response lenses
    , admApplication
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ElasticBeanstalk.Types

data CreateApplicationMessage = CreateApplicationMessage
    { _camApplicationName :: Text
    , _camDescription     :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'CreateApplicationMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'camApplicationName' @::@ 'Text'
--
-- * 'camDescription' @::@ 'Maybe' 'Text'
--
createApplicationMessage :: Text -- ^ 'camApplicationName'
                         -> CreateApplicationMessage
createApplicationMessage p1 = CreateApplicationMessage
    { _camApplicationName = p1
    , _camDescription     = Nothing
    }

-- | The name of the application. Constraint: This name must be unique within
-- your account. If the specified name already exists, the action returns an
-- InvalidParameterValue error.
camApplicationName :: Lens' CreateApplicationMessage Text
camApplicationName =
    lens _camApplicationName (\s a -> s { _camApplicationName = a })

-- | Describes the application.
camDescription :: Lens' CreateApplicationMessage (Maybe Text)
camDescription = lens _camDescription (\s a -> s { _camDescription = a })

instance ToPath CreateApplicationMessage where
    toPath = const "/"

instance ToQuery CreateApplicationMessage

instance AWSRequest CreateApplicationMessage where
    type Sv CreateApplicationMessage = ElasticBeanstalk
    type Rs CreateApplicationMessage = ApplicationDescriptionMessage

    request  = post "CreateApplication"
    response = const . xmlResponse $ const decodeCursor
