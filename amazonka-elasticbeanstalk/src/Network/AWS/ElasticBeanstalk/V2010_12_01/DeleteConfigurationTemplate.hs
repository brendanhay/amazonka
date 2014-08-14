{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}

{-# OPTIONS_GHC -fno-warn-unused-binds #-}

-- Module      : Network.AWS.ElasticBeanstalk.V2010_12_01.DeleteConfigurationTemplate
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes the specified configuration template. When you launch an
-- environment using a configuration template, the environment gets a copy of
-- the template. You can delete or modify the environment's copy of the
-- template without affecting the running environment.
-- https://elasticbeanstalk.us-east-1.amazon.com/?ApplicationName=SampleApp
-- &TemplateName=SampleAppTemplate &Operation=DeleteConfigurationTemplate
-- &AuthParams af9cf1b6-f25e-11df-8a78-9f77047e0d0c.
module Network.AWS.ElasticBeanstalk.V2010_12_01.DeleteConfigurationTemplate where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.Query
import Network.AWS.ElasticBeanstalk.V2010_12_01.Types
import Network.AWS.Prelude

data DeleteConfigurationTemplate = DeleteConfigurationTemplate
    { _dctmApplicationName :: Text
      -- ^ The name of the application to delete the configuration template
      -- from.
    , _dctmTemplateName :: Text
      -- ^ The name of the configuration template to delete.
    } deriving (Show, Generic)

makeLenses ''DeleteConfigurationTemplate

instance ToQuery DeleteConfigurationTemplate where
    toQuery = genericQuery def

data DeleteConfigurationTemplateResponse = DeleteConfigurationTemplateResponse
    deriving (Eq, Show, Generic)

makeLenses ''DeleteConfigurationTemplateResponse

instance AWSRequest DeleteConfigurationTemplate where
    type Sv DeleteConfigurationTemplate = ElasticBeanstalk
    type Rs DeleteConfigurationTemplate = DeleteConfigurationTemplateResponse

    request = post "DeleteConfigurationTemplate"
    response _ = nullaryResponse DeleteConfigurationTemplateResponse
