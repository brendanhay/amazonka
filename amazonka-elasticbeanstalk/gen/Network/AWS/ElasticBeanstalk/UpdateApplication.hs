{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElasticBeanstalk.UpdateApplication
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Updates the specified application to have the specified properties.
--
-- <http://docs.aws.amazon.com/elasticbeanstalk/latest/api/API_UpdateApplication.html>
module Network.AWS.ElasticBeanstalk.UpdateApplication
    (
    -- * Request
      UpdateApplication
    -- ** Request constructor
    , updateApplication
    -- ** Request lenses
    , uaApplicationName
    , uaDescription

    -- * Response
    , UpdateApplicationResponse
    -- ** Response constructor
    , updateApplicationResponse
    -- ** Response lenses
    , uarApplication
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ElasticBeanstalk.Types
import qualified GHC.Exts

data UpdateApplication = UpdateApplication
    { _uaApplicationName :: Text
    , _uaDescription     :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'UpdateApplication' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'uaApplicationName' @::@ 'Text'
--
-- * 'uaDescription' @::@ 'Maybe' 'Text'
--
updateApplication :: Text -- ^ 'uaApplicationName'
                  -> UpdateApplication
updateApplication p1 = UpdateApplication
    { _uaApplicationName = p1
    , _uaDescription     = Nothing
    }

-- | The name of the application to update. If no such application is found,
-- UpdateApplication returns an InvalidParameterValue error.
uaApplicationName :: Lens' UpdateApplication Text
uaApplicationName =
    lens _uaApplicationName (\s a -> s { _uaApplicationName = a })

-- | A new description for the application. Default: If not specified, AWS
-- Elastic Beanstalk does not update the description.
uaDescription :: Lens' UpdateApplication (Maybe Text)
uaDescription = lens _uaDescription (\s a -> s { _uaDescription = a })

newtype UpdateApplicationResponse = UpdateApplicationResponse
    { _uarApplication :: Maybe ApplicationDescription
    } deriving (Eq, Show, Generic)

-- | 'UpdateApplicationResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'uarApplication' @::@ 'Maybe' 'ApplicationDescription'
--
updateApplicationResponse :: UpdateApplicationResponse
updateApplicationResponse = UpdateApplicationResponse
    { _uarApplication = Nothing
    }

-- | The ApplicationDescription of the application.
uarApplication :: Lens' UpdateApplicationResponse (Maybe ApplicationDescription)
uarApplication = lens _uarApplication (\s a -> s { _uarApplication = a })

instance ToPath UpdateApplication where
    toPath = const "/"

instance ToQuery UpdateApplication

instance ToHeaders UpdateApplication

instance AWSRequest UpdateApplication where
    type Sv UpdateApplication = ElasticBeanstalk
    type Rs UpdateApplication = UpdateApplicationResponse

    request  = post "UpdateApplication"
    response = xmlResponse

instance FromXML UpdateApplicationResponse where
    parseXML c = UpdateApplicationResponse
        <$> c .:? "Application"
