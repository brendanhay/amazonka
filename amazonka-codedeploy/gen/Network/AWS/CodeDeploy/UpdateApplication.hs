{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CodeDeploy.UpdateApplication
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Changes an existing application's name.
--
-- <http://docs.aws.amazon.com/codedeploy/latest/APIReference/API_UpdateApplication.html>
module Network.AWS.CodeDeploy.UpdateApplication
    (
    -- * Request
      UpdateApplication
    -- ** Request constructor
    , updateApplication
    -- ** Request lenses
    , uaApplicationName
    , uaNewApplicationName

    -- * Response
    , UpdateApplicationResponse
    -- ** Response constructor
    , updateApplicationResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.CodeDeploy.Types
import qualified GHC.Exts

data UpdateApplication = UpdateApplication
    { _uaApplicationName    :: Maybe Text
    , _uaNewApplicationName :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'UpdateApplication' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'uaApplicationName' @::@ 'Maybe' 'Text'
--
-- * 'uaNewApplicationName' @::@ 'Maybe' 'Text'
--
updateApplication :: UpdateApplication
updateApplication = UpdateApplication
    { _uaApplicationName    = Nothing
    , _uaNewApplicationName = Nothing
    }

-- | The current name of the application that you want to change.
uaApplicationName :: Lens' UpdateApplication (Maybe Text)
uaApplicationName =
    lens _uaApplicationName (\s a -> s { _uaApplicationName = a })

-- | The new name that you want to change the application to.
uaNewApplicationName :: Lens' UpdateApplication (Maybe Text)
uaNewApplicationName =
    lens _uaNewApplicationName (\s a -> s { _uaNewApplicationName = a })

data UpdateApplicationResponse = UpdateApplicationResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'UpdateApplicationResponse' constructor.
updateApplicationResponse :: UpdateApplicationResponse
updateApplicationResponse = UpdateApplicationResponse

instance AWSRequest UpdateApplication where
    type Sv UpdateApplication = CodeDeploy
    type Rs UpdateApplication = UpdateApplicationResponse

    request  = post
    response = nullResponse UpdateApplicationResponse

instance ToPath UpdateApplication where
    toPath = const "/"

instance ToHeaders UpdateApplication

instance ToQuery UpdateApplication where
    toQuery = const mempty

instance ToJSON UpdateApplication where
    toJSON = genericToJSON jsonOptions
