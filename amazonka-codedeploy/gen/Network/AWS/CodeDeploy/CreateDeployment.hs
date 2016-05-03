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
-- Module      : Network.AWS.CodeDeploy.CreateDeployment
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deploys an application revision through the specified deployment group.
module Network.AWS.CodeDeploy.CreateDeployment
    (
    -- * Creating a Request
      createDeployment
    , CreateDeployment
    -- * Request Lenses
    , cdDeploymentConfigName
    , cdRevision
    , cdDescription
    , cdDeploymentGroupName
    , cdIgnoreApplicationStopFailures
    , cdApplicationName

    -- * Destructuring the Response
    , createDeploymentResponse
    , CreateDeploymentResponse
    -- * Response Lenses
    , cdrsDeploymentId
    , cdrsResponseStatus
    ) where

import           Network.AWS.CodeDeploy.Types
import           Network.AWS.CodeDeploy.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input of a create deployment operation.
--
-- /See:/ 'createDeployment' smart constructor.
data CreateDeployment = CreateDeployment'
    { _cdDeploymentConfigName          :: !(Maybe Text)
    , _cdRevision                      :: !(Maybe RevisionLocation)
    , _cdDescription                   :: !(Maybe Text)
    , _cdDeploymentGroupName           :: !(Maybe Text)
    , _cdIgnoreApplicationStopFailures :: !(Maybe Bool)
    , _cdApplicationName               :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateDeployment' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdDeploymentConfigName'
--
-- * 'cdRevision'
--
-- * 'cdDescription'
--
-- * 'cdDeploymentGroupName'
--
-- * 'cdIgnoreApplicationStopFailures'
--
-- * 'cdApplicationName'
createDeployment
    :: Text -- ^ 'cdApplicationName'
    -> CreateDeployment
createDeployment pApplicationName_ =
    CreateDeployment'
    { _cdDeploymentConfigName = Nothing
    , _cdRevision = Nothing
    , _cdDescription = Nothing
    , _cdDeploymentGroupName = Nothing
    , _cdIgnoreApplicationStopFailures = Nothing
    , _cdApplicationName = pApplicationName_
    }

-- | The name of a deployment configuration associated with the applicable
-- IAM user or AWS account.
--
-- If not specified, the value configured in the deployment group will be
-- used as the default. If the deployment group does not have a deployment
-- configuration associated with it, then CodeDeployDefault.OneAtATime will
-- be used by default.
cdDeploymentConfigName :: Lens' CreateDeployment (Maybe Text)
cdDeploymentConfigName = lens _cdDeploymentConfigName (\ s a -> s{_cdDeploymentConfigName = a});

-- | The type and location of the revision to deploy.
cdRevision :: Lens' CreateDeployment (Maybe RevisionLocation)
cdRevision = lens _cdRevision (\ s a -> s{_cdRevision = a});

-- | A comment about the deployment.
cdDescription :: Lens' CreateDeployment (Maybe Text)
cdDescription = lens _cdDescription (\ s a -> s{_cdDescription = a});

-- | The name of the deployment group.
cdDeploymentGroupName :: Lens' CreateDeployment (Maybe Text)
cdDeploymentGroupName = lens _cdDeploymentGroupName (\ s a -> s{_cdDeploymentGroupName = a});

-- | If set to true, then if the deployment causes the ApplicationStop
-- deployment lifecycle event to an instance to fail, the deployment to
-- that instance will not be considered to have failed at that point and
-- will continue on to the BeforeInstall deployment lifecycle event.
--
-- If set to false or not specified, then if the deployment causes the
-- ApplicationStop deployment lifecycle event to fail to an instance, the
-- deployment to that instance will stop, and the deployment to that
-- instance will be considered to have failed.
cdIgnoreApplicationStopFailures :: Lens' CreateDeployment (Maybe Bool)
cdIgnoreApplicationStopFailures = lens _cdIgnoreApplicationStopFailures (\ s a -> s{_cdIgnoreApplicationStopFailures = a});

-- | The name of an AWS CodeDeploy application associated with the applicable
-- IAM user or AWS account.
cdApplicationName :: Lens' CreateDeployment Text
cdApplicationName = lens _cdApplicationName (\ s a -> s{_cdApplicationName = a});

instance AWSRequest CreateDeployment where
        type Rs CreateDeployment = CreateDeploymentResponse
        request = postJSON codeDeploy
        response
          = receiveJSON
              (\ s h x ->
                 CreateDeploymentResponse' <$>
                   (x .?> "deploymentId") <*> (pure (fromEnum s)))

instance Hashable CreateDeployment

instance NFData CreateDeployment

instance ToHeaders CreateDeployment where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CodeDeploy_20141006.CreateDeployment" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateDeployment where
        toJSON CreateDeployment'{..}
          = object
              (catMaybes
                 [("deploymentConfigName" .=) <$>
                    _cdDeploymentConfigName,
                  ("revision" .=) <$> _cdRevision,
                  ("description" .=) <$> _cdDescription,
                  ("deploymentGroupName" .=) <$>
                    _cdDeploymentGroupName,
                  ("ignoreApplicationStopFailures" .=) <$>
                    _cdIgnoreApplicationStopFailures,
                  Just ("applicationName" .= _cdApplicationName)])

instance ToPath CreateDeployment where
        toPath = const "/"

instance ToQuery CreateDeployment where
        toQuery = const mempty

-- | Represents the output of a create deployment operation.
--
-- /See:/ 'createDeploymentResponse' smart constructor.
data CreateDeploymentResponse = CreateDeploymentResponse'
    { _cdrsDeploymentId   :: !(Maybe Text)
    , _cdrsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateDeploymentResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdrsDeploymentId'
--
-- * 'cdrsResponseStatus'
createDeploymentResponse
    :: Int -- ^ 'cdrsResponseStatus'
    -> CreateDeploymentResponse
createDeploymentResponse pResponseStatus_ =
    CreateDeploymentResponse'
    { _cdrsDeploymentId = Nothing
    , _cdrsResponseStatus = pResponseStatus_
    }

-- | A unique deployment ID.
cdrsDeploymentId :: Lens' CreateDeploymentResponse (Maybe Text)
cdrsDeploymentId = lens _cdrsDeploymentId (\ s a -> s{_cdrsDeploymentId = a});

-- | The response status code.
cdrsResponseStatus :: Lens' CreateDeploymentResponse Int
cdrsResponseStatus = lens _cdrsResponseStatus (\ s a -> s{_cdrsResponseStatus = a});

instance NFData CreateDeploymentResponse
