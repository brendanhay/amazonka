{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}

{-# OPTIONS_GHC -fno-warn-unused-binds #-}

-- Module      : Network.AWS.ElasticBeanstalk.V2010_12_01.DeleteApplication
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes the specified application along with all associated versions and
-- configurations. The application versions will not be deleted from your
-- Amazon S3 bucket. You cannot delete an application that has a running
-- environment.
-- https://elasticbeanstalk.us-east-1.amazon.com/?ApplicationName=SampleApp
-- &Operation=DeleteApplication &AuthParams
-- 1f155abd-f1d7-11df-8a78-9f77047e0d0c.
module Network.AWS.ElasticBeanstalk.V2010_12_01.DeleteApplication where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.Query
import Network.AWS.ElasticBeanstalk.V2010_12_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DeleteApplication' request.
deleteApplication :: Text -- ^ '_danApplicationName'
                  -> DeleteApplication
deleteApplication p1 = DeleteApplication
    { _danApplicationName = p1
    , _danTerminateEnvByForce = Nothing
    }

data DeleteApplication = DeleteApplication
    { _danApplicationName :: Text
      -- ^ The name of the application to delete.
    , _danTerminateEnvByForce :: Maybe Bool
      -- ^ When set to true, running environments will be terminated before
      -- deleting the application.
    } deriving (Show, Generic)

makeLenses ''DeleteApplication

instance ToQuery DeleteApplication where
    toQuery = genericQuery def

data DeleteApplicationResponse = DeleteApplicationResponse
    deriving (Eq, Show, Generic)

makeLenses ''DeleteApplicationResponse

instance AWSRequest DeleteApplication where
    type Sv DeleteApplication = ElasticBeanstalk
    type Rs DeleteApplication = DeleteApplicationResponse

    request = post "DeleteApplication"
    response _ = nullaryResponse DeleteApplicationResponse
