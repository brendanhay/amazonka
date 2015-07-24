{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.DeleteApplication
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified application along with all associated versions and
-- configurations. The application versions will not be deleted from your
-- Amazon S3 bucket.
--
-- You cannot delete an application that has a running environment.
--
-- <http://docs.aws.amazon.com/elasticbeanstalk/latest/api/API_DeleteApplication.html>
module Network.AWS.ElasticBeanstalk.DeleteApplication
    (
    -- * Request
      DeleteApplication
    -- ** Request constructor
    , deleteApplication
    -- ** Request lenses
    , daTerminateEnvByForce
    , daApplicationName

    -- * Response
    , DeleteApplicationResponse
    -- ** Response constructor
    , deleteApplicationResponse
    ) where

import           Network.AWS.ElasticBeanstalk.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | This documentation target is not reported in the API reference.
--
-- /See:/ 'deleteApplication' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'daTerminateEnvByForce'
--
-- * 'daApplicationName'
data DeleteApplication = DeleteApplication'
    { _daTerminateEnvByForce :: !(Maybe Bool)
    , _daApplicationName     :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteApplication' smart constructor.
deleteApplication :: Text -> DeleteApplication
deleteApplication pApplicationName_ =
    DeleteApplication'
    { _daTerminateEnvByForce = Nothing
    , _daApplicationName = pApplicationName_
    }

-- | When set to true, running environments will be terminated before
-- deleting the application.
daTerminateEnvByForce :: Lens' DeleteApplication (Maybe Bool)
daTerminateEnvByForce = lens _daTerminateEnvByForce (\ s a -> s{_daTerminateEnvByForce = a});

-- | The name of the application to delete.
daApplicationName :: Lens' DeleteApplication Text
daApplicationName = lens _daApplicationName (\ s a -> s{_daApplicationName = a});

instance AWSRequest DeleteApplication where
        type Sv DeleteApplication = ElasticBeanstalk
        type Rs DeleteApplication = DeleteApplicationResponse
        request = postQuery
        response = receiveNull DeleteApplicationResponse'

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
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteApplicationResponse' smart constructor.
deleteApplicationResponse :: DeleteApplicationResponse
deleteApplicationResponse = DeleteApplicationResponse'
