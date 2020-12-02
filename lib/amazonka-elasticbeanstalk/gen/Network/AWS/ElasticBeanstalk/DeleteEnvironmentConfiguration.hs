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
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the draft configuration associated with the running environment.
--
--
-- Updating a running environment with any configuration changes creates a draft configuration set. You can get the draft configuration using 'DescribeConfigurationSettings' while the update is in progress or if the update fails. The @DeploymentStatus@ for the draft configuration indicates whether the deployment is in process or has failed. The draft configuration remains in existence until it is deleted with this action.
--
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

import Network.AWS.ElasticBeanstalk.Types
import Network.AWS.ElasticBeanstalk.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Request to delete a draft environment configuration.
--
--
--
-- /See:/ 'deleteEnvironmentConfiguration' smart constructor.
data DeleteEnvironmentConfiguration = DeleteEnvironmentConfiguration'
  { _decApplicationName :: !Text
  , _decEnvironmentName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteEnvironmentConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'decApplicationName' - The name of the application the environment is associated with.
--
-- * 'decEnvironmentName' - The name of the environment to delete the draft configuration from.
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
decApplicationName = lens _decApplicationName (\ s a -> s{_decApplicationName = a})

-- | The name of the environment to delete the draft configuration from.
decEnvironmentName :: Lens' DeleteEnvironmentConfiguration Text
decEnvironmentName = lens _decEnvironmentName (\ s a -> s{_decEnvironmentName = a})

instance AWSRequest DeleteEnvironmentConfiguration
         where
        type Rs DeleteEnvironmentConfiguration =
             DeleteEnvironmentConfigurationResponse
        request = postQuery elasticBeanstalk
        response
          = receiveNull DeleteEnvironmentConfigurationResponse'

instance Hashable DeleteEnvironmentConfiguration
         where

instance NFData DeleteEnvironmentConfiguration where

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
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteEnvironmentConfigurationResponse' with the minimum fields required to make a request.
--
deleteEnvironmentConfigurationResponse
    :: DeleteEnvironmentConfigurationResponse
deleteEnvironmentConfigurationResponse = DeleteEnvironmentConfigurationResponse'


instance NFData
           DeleteEnvironmentConfigurationResponse
         where
