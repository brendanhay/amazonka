{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
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
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Deletes the specified configuration template.
--
-- <http://docs.aws.amazon.com/elasticbeanstalk/latest/api/API_DeleteConfigurationTemplate.html>
module Network.AWS.ElasticBeanstalk.DeleteConfigurationTemplate
    (
    -- * Request
      DeleteConfigurationTemplate
    -- ** Request constructor
    , deleteConfigurationTemplate
    -- ** Request lenses
    , dctApplicationName
    , dctTemplateName

    -- * Response
    , DeleteConfigurationTemplateResponse
    -- ** Response constructor
    , deleteConfigurationTemplateResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ElasticBeanstalk.Types
import qualified GHC.Exts

data DeleteConfigurationTemplate = DeleteConfigurationTemplate
    { _dctApplicationName :: Text
    , _dctTemplateName    :: Text
    } deriving (Eq, Ord, Show)

-- | 'DeleteConfigurationTemplate' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dctApplicationName' @::@ 'Text'
--
-- * 'dctTemplateName' @::@ 'Text'
--
deleteConfigurationTemplate :: Text -- ^ 'dctApplicationName'
                            -> Text -- ^ 'dctTemplateName'
                            -> DeleteConfigurationTemplate
deleteConfigurationTemplate p1 p2 = DeleteConfigurationTemplate
    { _dctApplicationName = p1
    , _dctTemplateName    = p2
    }

-- | The name of the application to delete the configuration template from.
dctApplicationName :: Lens' DeleteConfigurationTemplate Text
dctApplicationName =
    lens _dctApplicationName (\s a -> s { _dctApplicationName = a })

-- | The name of the configuration template to delete.
dctTemplateName :: Lens' DeleteConfigurationTemplate Text
dctTemplateName = lens _dctTemplateName (\s a -> s { _dctTemplateName = a })

data DeleteConfigurationTemplateResponse = DeleteConfigurationTemplateResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'DeleteConfigurationTemplateResponse' constructor.
deleteConfigurationTemplateResponse :: DeleteConfigurationTemplateResponse
deleteConfigurationTemplateResponse = DeleteConfigurationTemplateResponse

instance ToPath DeleteConfigurationTemplate where
    toPath = const "/"

instance ToQuery DeleteConfigurationTemplate where
    toQuery DeleteConfigurationTemplate{..} = mconcat
        [ "ApplicationName" =? _dctApplicationName
        , "TemplateName"    =? _dctTemplateName
        ]

instance ToHeaders DeleteConfigurationTemplate

instance AWSRequest DeleteConfigurationTemplate where
    type Sv DeleteConfigurationTemplate = ElasticBeanstalk
    type Rs DeleteConfigurationTemplate = DeleteConfigurationTemplateResponse

    request  = post "DeleteConfigurationTemplate"
    response = nullResponse DeleteConfigurationTemplateResponse
