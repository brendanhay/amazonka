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

-- Module      : Network.AWS.ElasticBeanstalk.DeleteConfigurationTemplate
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes the specified configuration template.
module Network.AWS.ElasticBeanstalk.DeleteConfigurationTemplate
    (
    -- * Request
      DeleteConfigurationTemplateMessage
    -- ** Request constructor
    , deleteConfigurationTemplate
    -- ** Request lenses
    , dctmApplicationName
    , dctmTemplateName

    -- * Response
    , DeleteConfigurationTemplateResponse
    -- ** Response constructor
    , deleteConfigurationTemplateResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ElasticBeanstalk.Types

data DeleteConfigurationTemplateMessage = DeleteConfigurationTemplateMessage
    { _dctmApplicationName :: Text
    , _dctmTemplateName    :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'DeleteConfigurationTemplateMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dctmApplicationName' @::@ 'Text'
--
-- * 'dctmTemplateName' @::@ 'Text'
--
deleteConfigurationTemplate :: Text -- ^ 'dctmApplicationName'
                            -> Text -- ^ 'dctmTemplateName'
                            -> DeleteConfigurationTemplateMessage
deleteConfigurationTemplate p1 p2 = DeleteConfigurationTemplateMessage
    { _dctmApplicationName = p1
    , _dctmTemplateName    = p2
    }

-- | The name of the application to delete the configuration template from.
dctmApplicationName :: Lens' DeleteConfigurationTemplateMessage Text
dctmApplicationName =
    lens _dctmApplicationName (\s a -> s { _dctmApplicationName = a })

-- | The name of the configuration template to delete.
dctmTemplateName :: Lens' DeleteConfigurationTemplateMessage Text
dctmTemplateName = lens _dctmTemplateName (\s a -> s { _dctmTemplateName = a })

instance ToQuery DeleteConfigurationTemplateMessage

instance ToPath DeleteConfigurationTemplateMessage where
    toPath = const "/"

data DeleteConfigurationTemplateResponse = DeleteConfigurationTemplateResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'DeleteConfigurationTemplateResponse' constructor.
deleteConfigurationTemplateResponse :: DeleteConfigurationTemplateResponse
deleteConfigurationTemplateResponse = DeleteConfigurationTemplateResponse

instance FromXML DeleteConfigurationTemplateResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DeleteConfigurationTemplateResponse"

instance AWSRequest DeleteConfigurationTemplateMessage where
    type Sv DeleteConfigurationTemplateMessage = ElasticBeanstalk
    type Rs DeleteConfigurationTemplateMessage = DeleteConfigurationTemplateResponse

    request  = post "DeleteConfigurationTemplate"
    response = nullaryResponse DeleteConfigurationTemplateResponse
