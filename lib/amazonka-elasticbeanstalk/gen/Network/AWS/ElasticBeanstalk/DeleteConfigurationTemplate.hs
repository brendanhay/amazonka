{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.DeleteConfigurationTemplate
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified configuration template.
--
--
module Network.AWS.ElasticBeanstalk.DeleteConfigurationTemplate
    (
    -- * Creating a Request
      deleteConfigurationTemplate
    , DeleteConfigurationTemplate
    -- * Request Lenses
    , dctApplicationName
    , dctTemplateName

    -- * Destructuring the Response
    , deleteConfigurationTemplateResponse
    , DeleteConfigurationTemplateResponse
    ) where

import Network.AWS.ElasticBeanstalk.Types
import Network.AWS.ElasticBeanstalk.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Request to delete a configuration template.
--
--
--
-- /See:/ 'deleteConfigurationTemplate' smart constructor.
data DeleteConfigurationTemplate = DeleteConfigurationTemplate'
  { _dctApplicationName :: !Text
  , _dctTemplateName    :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteConfigurationTemplate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dctApplicationName' - The name of the application to delete the configuration template from.
--
-- * 'dctTemplateName' - The name of the configuration template to delete.
deleteConfigurationTemplate
    :: Text -- ^ 'dctApplicationName'
    -> Text -- ^ 'dctTemplateName'
    -> DeleteConfigurationTemplate
deleteConfigurationTemplate pApplicationName_ pTemplateName_ =
  DeleteConfigurationTemplate'
    {_dctApplicationName = pApplicationName_, _dctTemplateName = pTemplateName_}


-- | The name of the application to delete the configuration template from.
dctApplicationName :: Lens' DeleteConfigurationTemplate Text
dctApplicationName = lens _dctApplicationName (\ s a -> s{_dctApplicationName = a})

-- | The name of the configuration template to delete.
dctTemplateName :: Lens' DeleteConfigurationTemplate Text
dctTemplateName = lens _dctTemplateName (\ s a -> s{_dctTemplateName = a})

instance AWSRequest DeleteConfigurationTemplate where
        type Rs DeleteConfigurationTemplate =
             DeleteConfigurationTemplateResponse
        request = postQuery elasticBeanstalk
        response
          = receiveNull DeleteConfigurationTemplateResponse'

instance Hashable DeleteConfigurationTemplate where

instance NFData DeleteConfigurationTemplate where

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
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteConfigurationTemplateResponse' with the minimum fields required to make a request.
--
deleteConfigurationTemplateResponse
    :: DeleteConfigurationTemplateResponse
deleteConfigurationTemplateResponse = DeleteConfigurationTemplateResponse'


instance NFData DeleteConfigurationTemplateResponse
         where
