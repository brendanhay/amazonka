{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.DeleteConfigurationTemplate
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified configuration template.
--
-- When you launch an environment using a configuration template, the
-- environment gets a copy of the template. You can delete or modify the
-- environment\'s copy of the template without affecting the running
-- environment.
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

import           Network.AWS.ElasticBeanstalk.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | This documentation target is not reported in the API reference.
--
-- /See:/ 'deleteConfigurationTemplate' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dctApplicationName'
--
-- * 'dctTemplateName'
data DeleteConfigurationTemplate = DeleteConfigurationTemplate'
    { _dctApplicationName :: !Text
    , _dctTemplateName    :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteConfigurationTemplate' smart constructor.
deleteConfigurationTemplate :: Text -> Text -> DeleteConfigurationTemplate
deleteConfigurationTemplate pApplicationName_ pTemplateName_ =
    DeleteConfigurationTemplate'
    { _dctApplicationName = pApplicationName_
    , _dctTemplateName = pTemplateName_
    }

-- | The name of the application to delete the configuration template from.
dctApplicationName :: Lens' DeleteConfigurationTemplate Text
dctApplicationName = lens _dctApplicationName (\ s a -> s{_dctApplicationName = a});

-- | The name of the configuration template to delete.
dctTemplateName :: Lens' DeleteConfigurationTemplate Text
dctTemplateName = lens _dctTemplateName (\ s a -> s{_dctTemplateName = a});

instance AWSRequest DeleteConfigurationTemplate where
        type Sv DeleteConfigurationTemplate =
             ElasticBeanstalk
        type Rs DeleteConfigurationTemplate =
             DeleteConfigurationTemplateResponse
        request = post
        response
          = receiveNull DeleteConfigurationTemplateResponse'

instance ToHeaders DeleteConfigurationTemplate where
        toHeaders = const mempty

instance ToPath DeleteConfigurationTemplate where
        toPath = const "/"

instance ToQuery DeleteConfigurationTemplate where
        toQuery DeleteConfigurationTemplate'{..}
          = mconcat
              ["Action" =:
                 ("DeleteConfigurationTemplate" :: ByteString),
               "Version" =: ("2010-12-01" :: ByteString),
               "ApplicationName" =: _dctApplicationName,
               "TemplateName" =: _dctTemplateName]

-- | /See:/ 'deleteConfigurationTemplateResponse' smart constructor.
data DeleteConfigurationTemplateResponse =
    DeleteConfigurationTemplateResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteConfigurationTemplateResponse' smart constructor.
deleteConfigurationTemplateResponse :: DeleteConfigurationTemplateResponse
deleteConfigurationTemplateResponse = DeleteConfigurationTemplateResponse'
