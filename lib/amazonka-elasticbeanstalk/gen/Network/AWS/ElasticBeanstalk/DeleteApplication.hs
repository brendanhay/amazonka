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
-- Module      : Network.AWS.ElasticBeanstalk.DeleteApplication
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified application along with all associated versions and configurations. The application versions will not be deleted from your Amazon S3 bucket.
--
--
module Network.AWS.ElasticBeanstalk.DeleteApplication
    (
    -- * Creating a Request
      deleteApplication
    , DeleteApplication
    -- * Request Lenses
    , daTerminateEnvByForce
    , daApplicationName

    -- * Destructuring the Response
    , deleteApplicationResponse
    , DeleteApplicationResponse
    ) where

import Network.AWS.ElasticBeanstalk.Types
import Network.AWS.ElasticBeanstalk.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Request to delete an application.
--
--
--
-- /See:/ 'deleteApplication' smart constructor.
data DeleteApplication = DeleteApplication'
  { _daTerminateEnvByForce :: !(Maybe Bool)
  , _daApplicationName     :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteApplication' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'daTerminateEnvByForce' - When set to true, running environments will be terminated before deleting the application.
--
-- * 'daApplicationName' - The name of the application to delete.
deleteApplication
    :: Text -- ^ 'daApplicationName'
    -> DeleteApplication
deleteApplication pApplicationName_ =
  DeleteApplication'
    {_daTerminateEnvByForce = Nothing, _daApplicationName = pApplicationName_}


-- | When set to true, running environments will be terminated before deleting the application.
daTerminateEnvByForce :: Lens' DeleteApplication (Maybe Bool)
daTerminateEnvByForce = lens _daTerminateEnvByForce (\ s a -> s{_daTerminateEnvByForce = a})

-- | The name of the application to delete.
daApplicationName :: Lens' DeleteApplication Text
daApplicationName = lens _daApplicationName (\ s a -> s{_daApplicationName = a})

instance AWSRequest DeleteApplication where
        type Rs DeleteApplication = DeleteApplicationResponse
        request = postQuery elasticBeanstalk
        response = receiveNull DeleteApplicationResponse'

instance Hashable DeleteApplication where

instance NFData DeleteApplication where

instance ToHeaders DeleteApplication where
        toHeaders = const mempty

instance ToPath DeleteApplication where
        toPath = const "/"

instance ToQuery DeleteApplication where
        toQuery DeleteApplication'{..}
          = mconcat
              ["Action" =: ("DeleteApplication" :: ByteString),
               "Version" =: ("2010-12-01" :: ByteString),
               "TerminateEnvByForce" =: _daTerminateEnvByForce,
               "ApplicationName" =: _daApplicationName]

-- | /See:/ 'deleteApplicationResponse' smart constructor.
data DeleteApplicationResponse =
  DeleteApplicationResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteApplicationResponse' with the minimum fields required to make a request.
--
deleteApplicationResponse
    :: DeleteApplicationResponse
deleteApplicationResponse = DeleteApplicationResponse'


instance NFData DeleteApplicationResponse where
