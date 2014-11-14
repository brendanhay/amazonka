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

-- Module      : Network.AWS.CodeDeploy.CreateApplication
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates a new application.
module Network.AWS.CodeDeploy.CreateApplication
    (
    -- * Request
      CreateApplication
    -- ** Request constructor
    , createApplication
    -- ** Request lenses
    , caApplicationName

    -- * Response
    , CreateApplicationResponse
    -- ** Response constructor
    , createApplicationResponse
    -- ** Response lenses
    , carApplicationId
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.CodeDeploy.Types

newtype CreateApplication = CreateApplication
    { _caApplicationName :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid, IsString)

-- | 'CreateApplication' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'caApplicationName' @::@ 'Text'
--
createApplication :: Text -- ^ 'caApplicationName'
                  -> CreateApplication
createApplication p1 = CreateApplication
    { _caApplicationName = p1
    }

-- | The name of the application. This name must be unique within the AWS user
-- account.
caApplicationName :: Lens' CreateApplication Text
caApplicationName =
    lens _caApplicationName (\s a -> s { _caApplicationName = a })

instance ToPath CreateApplication where
    toPath = const "/"

instance ToQuery CreateApplication where
    toQuery = const mempty

instance ToHeaders CreateApplication

instance ToBody CreateApplication where
    toBody = toBody . encode . _caApplicationName

newtype CreateApplicationResponse = CreateApplicationResponse
    { _carApplicationId :: Maybe Text
    } deriving (Eq, Ord, Show, Generic, Monoid)

-- | 'CreateApplicationResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'carApplicationId' @::@ 'Maybe' 'Text'
--
createApplicationResponse :: CreateApplicationResponse
createApplicationResponse = CreateApplicationResponse
    { _carApplicationId = Nothing
    }

-- | A unique application ID.
carApplicationId :: Lens' CreateApplicationResponse (Maybe Text)
carApplicationId = lens _carApplicationId (\s a -> s { _carApplicationId = a })

instance AWSRequest CreateApplication where
    type Sv CreateApplication = CodeDeploy
    type Rs CreateApplication = CreateApplicationResponse

    request  = post
    response = jsonResponse $ \h o -> CreateApplicationResponse
        <$> o .: "applicationId"
