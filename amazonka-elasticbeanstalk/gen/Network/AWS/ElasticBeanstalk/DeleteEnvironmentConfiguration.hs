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
-- Module      : Network.AWS.ElasticBeanstalk.DeleteEnvironmentConfiguration
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the draft configuration associated with the running environment.
--
-- Updating a running environment with any configuration changes creates a
-- draft configuration set. You can get the draft configuration using
-- DescribeConfigurationSettings while the update is in progress or if the
-- update fails. The 'DeploymentStatus' for the draft configuration
-- indicates whether the deployment is in process or has failed. The draft
-- configuration remains in existence until it is deleted with this action.
--
-- /See:/ <http://docs.aws.amazon.com/elasticbeanstalk/latest/api/API_DeleteEnvironmentConfiguration.html AWS API Reference> for DeleteEnvironmentConfiguration.
module Network.AWS.ElasticBeanstalk.DeleteEnvironmentConfiguration
    (
    -- * Creating a Request
      deleteEnvironmentConfiguration
    , DeleteEnvironmentConfiguration
    -- * Request Lenses
    , decApplicationName
    , decEnvironmentName

    -- * Destructuring the Response
    , deleteEnvironmentConfigurationResponse
    , DeleteEnvironmentConfigurationResponse
    ) where

import           Network.AWS.ElasticBeanstalk.Types
import           Network.AWS.ElasticBeanstalk.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | This documentation target is not reported in the API reference.
--
-- /See:/ 'deleteEnvironmentConfiguration' smart constructor.
data DeleteEnvironmentConfiguration = DeleteEnvironmentConfiguration'
    { _decApplicationName :: !Text
    , _decEnvironmentName :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeleteEnvironmentConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'decApplicationName'
--
-- * 'decEnvironmentName'
deleteEnvironmentConfiguration
    :: Text -- ^ 'decApplicationName'
    -> Text -- ^ 'decEnvironmentName'
    -> DeleteEnvironmentConfiguration
deleteEnvironmentConfiguration pApplicationName_ pEnvironmentName_ =
    DeleteEnvironmentConfiguration'
    { _decApplicationName = pApplicationName_
    , _decEnvironmentName = pEnvironmentName_
    }

-- | The name of the application the environment is associated with.
decApplicationName :: Lens' DeleteEnvironmentConfiguration Text
decApplicationName = lens _decApplicationName (\ s a -> s{_decApplicationName = a});

-- | The name of the environment to delete the draft configuration from.
decEnvironmentName :: Lens' DeleteEnvironmentConfiguration Text
decEnvironmentName = lens _decEnvironmentName (\ s a -> s{_decEnvironmentName = a});

instance AWSRequest DeleteEnvironmentConfiguration
         where
        type Rs DeleteEnvironmentConfiguration =
             DeleteEnvironmentConfigurationResponse
        request = postQuery elasticBeanstalk
        response
          = receiveNull DeleteEnvironmentConfigurationResponse'

instance ToHeaders DeleteEnvironmentConfiguration
         where
        toHeaders = const mempty

instance ToPath DeleteEnvironmentConfiguration where
        toPath = const "/"

instance ToQuery DeleteEnvironmentConfiguration where
        toQuery DeleteEnvironmentConfiguration'{..}
          = mconcat
              ["Action" =:
                 ("DeleteEnvironmentConfiguration" :: ByteString),
               "Version" =: ("2010-12-01" :: ByteString),
               "ApplicationName" =: _decApplicationName,
               "EnvironmentName" =: _decEnvironmentName]

-- | /See:/ 'deleteEnvironmentConfigurationResponse' smart constructor.
data DeleteEnvironmentConfigurationResponse =
    DeleteEnvironmentConfigurationResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeleteEnvironmentConfigurationResponse' with the minimum fields required to make a request.
--
deleteEnvironmentConfigurationResponse
    :: DeleteEnvironmentConfigurationResponse
deleteEnvironmentConfigurationResponse =
    DeleteEnvironmentConfigurationResponse'
