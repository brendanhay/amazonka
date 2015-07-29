{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.DeleteEnvironmentConfiguration
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Deletes the draft configuration associated with the running environment.
--
-- Updating a running environment with any configuration changes creates a
-- draft configuration set. You can get the draft configuration using
-- DescribeConfigurationSettings while the update is in progress or if the
-- update fails. The @DeploymentStatus@ for the draft configuration
-- indicates whether the deployment is in process or has failed. The draft
-- configuration remains in existence until it is deleted with this action.
--
-- <http://docs.aws.amazon.com/elasticbeanstalk/latest/api/API_DeleteEnvironmentConfiguration.html>
module Network.AWS.ElasticBeanstalk.DeleteEnvironmentConfiguration
    (
    -- * Request
      DeleteEnvironmentConfiguration
    -- ** Request constructor
    , deleteEnvironmentConfiguration
    -- ** Request lenses
    , decApplicationName
    , decEnvironmentName

    -- * Response
    , DeleteEnvironmentConfigurationResponse
    -- ** Response constructor
    , deleteEnvironmentConfigurationResponse
    ) where

import           Network.AWS.ElasticBeanstalk.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | This documentation target is not reported in the API reference.
--
-- /See:/ 'deleteEnvironmentConfiguration' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'decApplicationName'
--
-- * 'decEnvironmentName'
data DeleteEnvironmentConfiguration = DeleteEnvironmentConfiguration'
    { _decApplicationName :: !Text
    , _decEnvironmentName :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteEnvironmentConfiguration' smart constructor.
deleteEnvironmentConfiguration :: Text -> Text -> DeleteEnvironmentConfiguration
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
        type Sv DeleteEnvironmentConfiguration =
             ElasticBeanstalk
        type Rs DeleteEnvironmentConfiguration =
             DeleteEnvironmentConfigurationResponse
        request = postQuery
        response
          = receiveNull DeleteEnvironmentConfigurationResponse'

instance ToHeaders DeleteEnvironmentConfiguration
         where
        toHeaders = const mempty

instance ToPath DeleteEnvironmentConfiguration where
        toPath = const mempty

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

-- | 'DeleteEnvironmentConfigurationResponse' smart constructor.
deleteEnvironmentConfigurationResponse :: DeleteEnvironmentConfigurationResponse
deleteEnvironmentConfigurationResponse =
    DeleteEnvironmentConfigurationResponse'
