{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElasticBeanstalk.DeleteConfigurationTemplate
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
module Network.AWS.ElasticBeanstalk.DeleteConfigurationTemplate
    (
    -- * Request
      DeleteConfigurationTemplate
    -- ** Request constructor
    , mkDeleteConfigurationTemplate
    -- ** Request lenses
    , dctApplicationName
    , dctTemplateName

    -- * Response
    , DeleteConfigurationTemplateResponse
    -- ** Response constructor
    , mkDeleteConfigurationTemplateResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.ElasticBeanstalk.Types
import Network.AWS.Prelude

-- | This documentation target is not reported in the API reference.
data DeleteConfigurationTemplate = DeleteConfigurationTemplate
    { _dctApplicationName :: !Text
    , _dctTemplateName :: !Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteConfigurationTemplate' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ApplicationName ::@ @Text@
--
-- * @TemplateName ::@ @Text@
--
mkDeleteConfigurationTemplate :: Text -- ^ 'dctApplicationName'
                              -> Text -- ^ 'dctTemplateName'
                              -> DeleteConfigurationTemplate
mkDeleteConfigurationTemplate p1 p2 = DeleteConfigurationTemplate
    { _dctApplicationName = p1
    , _dctTemplateName = p2
    }

-- | The name of the application to delete the configuration template from.
dctApplicationName :: Lens' DeleteConfigurationTemplate Text
dctApplicationName =
    lens _dctApplicationName (\s a -> s { _dctApplicationName = a })

-- | The name of the configuration template to delete.
dctTemplateName :: Lens' DeleteConfigurationTemplate Text
dctTemplateName = lens _dctTemplateName (\s a -> s { _dctTemplateName = a })

instance ToQuery DeleteConfigurationTemplate where
    toQuery = genericQuery def

data DeleteConfigurationTemplateResponse = DeleteConfigurationTemplateResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteConfigurationTemplateResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkDeleteConfigurationTemplateResponse :: DeleteConfigurationTemplateResponse
mkDeleteConfigurationTemplateResponse = DeleteConfigurationTemplateResponse

instance AWSRequest DeleteConfigurationTemplate where
    type Sv DeleteConfigurationTemplate = ElasticBeanstalk
    type Rs DeleteConfigurationTemplate = DeleteConfigurationTemplateResponse

    request = post "DeleteConfigurationTemplate"
    response _ = nullaryResponse DeleteConfigurationTemplateResponse
