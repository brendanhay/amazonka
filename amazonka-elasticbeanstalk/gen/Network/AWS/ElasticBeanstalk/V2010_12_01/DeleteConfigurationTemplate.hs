{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
module Network.AWS.ElasticBeanstalk.V2010_12_01.DeleteConfigurationTemplate
    (
    -- * Request
      DeleteConfigurationTemplate
    -- ** Request constructor
    , deleteConfigurationTemplate
    -- ** Request lenses
    , dctmApplicationName
    , dctmTemplateName

    -- * Response
    , DeleteConfigurationTemplateResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.ElasticBeanstalk.V2010_12_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DeleteConfigurationTemplate' request.
deleteConfigurationTemplate :: Text -- ^ 'dctmApplicationName'
                            -> Text -- ^ 'dctmTemplateName'
                            -> DeleteConfigurationTemplate
deleteConfigurationTemplate p1 p2 = DeleteConfigurationTemplate
    { _dctmApplicationName = p1
    , _dctmTemplateName = p2
    }

data DeleteConfigurationTemplate = DeleteConfigurationTemplate
    { _dctmApplicationName :: Text
      -- ^ The name of the application to delete the configuration template
      -- from.
    , _dctmTemplateName :: Text
      -- ^ The name of the configuration template to delete.
    } deriving (Show, Generic)

-- | The name of the application to delete the configuration template from.
dctmApplicationName
    :: Functor f
    => (Text
    -> f (Text))
    -> DeleteConfigurationTemplate
    -> f DeleteConfigurationTemplate
dctmApplicationName f x =
    (\y -> x { _dctmApplicationName = y })
       <$> f (_dctmApplicationName x)
{-# INLINE dctmApplicationName #-}

-- | The name of the configuration template to delete.
dctmTemplateName
    :: Functor f
    => (Text
    -> f (Text))
    -> DeleteConfigurationTemplate
    -> f DeleteConfigurationTemplate
dctmTemplateName f x =
    (\y -> x { _dctmTemplateName = y })
       <$> f (_dctmTemplateName x)
{-# INLINE dctmTemplateName #-}

instance ToQuery DeleteConfigurationTemplate where
    toQuery = genericQuery def

data DeleteConfigurationTemplateResponse = DeleteConfigurationTemplateResponse
    deriving (Eq, Show, Generic)

instance AWSRequest DeleteConfigurationTemplate where
    type Sv DeleteConfigurationTemplate = ElasticBeanstalk
    type Rs DeleteConfigurationTemplate = DeleteConfigurationTemplateResponse

    request = post "DeleteConfigurationTemplate"
    response _ = nullaryResponse DeleteConfigurationTemplateResponse
