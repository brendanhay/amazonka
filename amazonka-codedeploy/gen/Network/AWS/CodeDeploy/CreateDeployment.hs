{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.CodeDeploy.CreateDeployment
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Deploys an application revision through the specified deployment group.
--
-- <http://docs.aws.amazon.com/codedeploy/latest/APIReference/API_CreateDeployment.html>
module Network.AWS.CodeDeploy.CreateDeployment
    (
    -- * Request
      CreateDeployment
    -- ** Request constructor
    , createDeployment
    -- ** Request lenses
    , cdDeploymentConfigName
    , cdRevision
    , cdDescription
    , cdIgnoreApplicationStopFailures
    , cdDeploymentGroupName
    , cdApplicationName

    -- * Response
    , CreateDeploymentResponse
    -- ** Response constructor
    , createDeploymentResponse
    -- ** Response lenses
    , cdrDeploymentId
    , cdrStatus
    ) where

import           Network.AWS.CodeDeploy.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input of a create deployment operation.
--
-- /See:/ 'createDeployment' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cdDeploymentConfigName'
--
-- * 'cdRevision'
--
-- * 'cdDescription'
--
-- * 'cdIgnoreApplicationStopFailures'
--
-- * 'cdDeploymentGroupName'
--
-- * 'cdApplicationName'
data CreateDeployment = CreateDeployment'
    { _cdDeploymentConfigName          :: !(Maybe Text)
    , _cdRevision                      :: !(Maybe RevisionLocation)
    , _cdDescription                   :: !(Maybe Text)
    , _cdIgnoreApplicationStopFailures :: !(Maybe Bool)
    , _cdDeploymentGroupName           :: !(Maybe Text)
    , _cdApplicationName               :: !Text
    } deriving (Eq,Read,Show)

-- | 'CreateDeployment' smart constructor.
createDeployment :: Text -> CreateDeployment
createDeployment pApplicationName =
    CreateDeployment'
    { _cdDeploymentConfigName = Nothing
    , _cdRevision = Nothing
    , _cdDescription = Nothing
    , _cdIgnoreApplicationStopFailures = Nothing
    , _cdDeploymentGroupName = Nothing
    , _cdApplicationName = pApplicationName
    }

-- | The name of an existing deployment configuration associated with the
-- applicable IAM user or AWS account.
--
-- If not specified, the value configured in the deployment group will be
-- used as the default. If the deployment group does not have a deployment
-- configuration associated with it, then CodeDeployDefault.OneAtATime will
-- be used by default.
cdDeploymentConfigName :: Lens' CreateDeployment (Maybe Text)
cdDeploymentConfigName = lens _cdDeploymentConfigName (\ s a -> s{_cdDeploymentConfigName = a});

-- | The type of revision to deploy, along with information about the
-- revision\'s location.
cdRevision :: Lens' CreateDeployment (Maybe RevisionLocation)
cdRevision = lens _cdRevision (\ s a -> s{_cdRevision = a});

-- | A comment about the deployment.
cdDescription :: Lens' CreateDeployment (Maybe Text)
cdDescription = lens _cdDescription (\ s a -> s{_cdDescription = a});

-- | If set to true, then if the deployment causes the ApplicationStop
-- deployment lifecycle event to fail to a specific instance, the
-- deployment will not be considered to have failed to that instance at
-- that point and will continue on to the BeforeInstall deployment
-- lifecycle event.
--
-- If set to false or not specified, then if the deployment causes the
-- ApplicationStop deployment lifecycle event to fail to a specific
-- instance, the deployment will stop to that instance, and the deployment
-- to that instance will be considered to have failed.
cdIgnoreApplicationStopFailures :: Lens' CreateDeployment (Maybe Bool)
cdIgnoreApplicationStopFailures = lens _cdIgnoreApplicationStopFailures (\ s a -> s{_cdIgnoreApplicationStopFailures = a});

-- | The deployment group\'s name.
cdDeploymentGroupName :: Lens' CreateDeployment (Maybe Text)
cdDeploymentGroupName = lens _cdDeploymentGroupName (\ s a -> s{_cdDeploymentGroupName = a});

-- | The name of an existing AWS CodeDeploy application associated with the
-- applicable IAM user or AWS account.
cdApplicationName :: Lens' CreateDeployment Text
cdApplicationName = lens _cdApplicationName (\ s a -> s{_cdApplicationName = a});

instance AWSRequest CreateDeployment where
        type Sv CreateDeployment = CodeDeploy
        type Rs CreateDeployment = CreateDeploymentResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 CreateDeploymentResponse' <$>
                   (x .?> "deploymentId") <*> (pure s))

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
              ["deploymentConfigName" .= _cdDeploymentConfigName,
               "revision" .= _cdRevision,
               "description" .= _cdDescription,
               "ignoreApplicationStopFailures" .=
                 _cdIgnoreApplicationStopFailures,
               "deploymentGroupName" .= _cdDeploymentGroupName,
               "applicationName" .= _cdApplicationName]

instance ToPath CreateDeployment where
        toPath = const "/"

instance ToQuery CreateDeployment where
        toQuery = const mempty

-- | Represents the output of a create deployment operation.
--
-- /See:/ 'createDeploymentResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cdrDeploymentId'
--
-- * 'cdrStatus'
data CreateDeploymentResponse = CreateDeploymentResponse'
    { _cdrDeploymentId :: !(Maybe Text)
    , _cdrStatus       :: !Status
    } deriving (Eq,Show)

-- | 'CreateDeploymentResponse' smart constructor.
createDeploymentResponse :: Status -> CreateDeploymentResponse
createDeploymentResponse pStatus =
    CreateDeploymentResponse'
    { _cdrDeploymentId = Nothing
    , _cdrStatus = pStatus
    }

-- | A unique deployment ID.
cdrDeploymentId :: Lens' CreateDeploymentResponse (Maybe Text)
cdrDeploymentId = lens _cdrDeploymentId (\ s a -> s{_cdrDeploymentId = a});

-- | FIXME: Undocumented member.
cdrStatus :: Lens' CreateDeploymentResponse Status
cdrStatus = lens _cdrStatus (\ s a -> s{_cdrStatus = a});
