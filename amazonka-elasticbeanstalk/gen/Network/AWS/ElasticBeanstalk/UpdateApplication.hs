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
module Network.AWS.ElasticBeanstalk.UpdateApplication
    (
    -- * Request
      UpdateApplicationMessage
    -- ** Request constructor
    , updateApplicationMessage
    -- ** Request lenses
    , uamApplicationName
    , uamDescription

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

data UpdateApplicationMessage = UpdateApplicationMessage
    { _uamApplicationName :: Text
    , _uamDescription     :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'UpdateApplicationMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'uamApplicationName' @::@ 'Text'
--
-- * 'uamDescription' @::@ 'Maybe' 'Text'
--
updateApplicationMessage :: Text -- ^ 'uamApplicationName'
                         -> UpdateApplicationMessage
updateApplicationMessage p1 = UpdateApplicationMessage
    { _uamApplicationName = p1
    , _uamDescription     = Nothing
    }

-- | The name of the application to update. If no such application is found,
-- UpdateApplication returns an InvalidParameterValue error.
uamApplicationName :: Lens' UpdateApplicationMessage Text
uamApplicationName =
    lens _uamApplicationName (\s a -> s { _uamApplicationName = a })

-- | A new description for the application. Default: If not specified, AWS
-- Elastic Beanstalk does not update the description.
uamDescription :: Lens' UpdateApplicationMessage (Maybe Text)
uamDescription = lens _uamDescription (\s a -> s { _uamDescription = a })

instance ToPath UpdateApplicationMessage where
    toPath = const "/"

instance ToQuery UpdateApplicationMessage

instance AWSRequest UpdateApplicationMessage where
    type Sv UpdateApplicationMessage = ElasticBeanstalk
    type Rs UpdateApplicationMessage = ApplicationDescriptionMessage

    request  = post "UpdateApplication"
    response = const . xmlResponse $ const decodeCursor
