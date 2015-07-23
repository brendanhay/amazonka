{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.CreateDeployment
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Deploys an application revision through the specified deployment group.
--
-- <http://docs.aws.amazon.com/codedeploy/latest/APIReference/API_CreateDeployment.html>
module Network.AWS.CodeDeploy.CreateDeployment
    (
    -- * Request
      CreateDeployment
    -- ** Request constructor
    , createDeployment
    -- ** Request lenses
    , cdrqDeploymentConfigName
    , cdrqRevision
    , cdrqDescription
    , cdrqIgnoreApplicationStopFailures
    , cdrqDeploymentGroupName
    , cdrqApplicationName

    -- * Response
    , CreateDeploymentResponse
    -- ** Response constructor
    , createDeploymentResponse
    -- ** Response lenses
    , cdrsDeploymentId
    , cdrsStatus
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
-- * 'cdrqDeploymentConfigName'
--
-- * 'cdrqRevision'
--
-- * 'cdrqDescription'
--
-- * 'cdrqIgnoreApplicationStopFailures'
--
-- * 'cdrqDeploymentGroupName'
--
-- * 'cdrqApplicationName'
data CreateDeployment = CreateDeployment'
    { _cdrqDeploymentConfigName          :: !(Maybe Text)
    , _cdrqRevision                      :: !(Maybe RevisionLocation)
    , _cdrqDescription                   :: !(Maybe Text)
    , _cdrqIgnoreApplicationStopFailures :: !(Maybe Bool)
    , _cdrqDeploymentGroupName           :: !(Maybe Text)
    , _cdrqApplicationName               :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateDeployment' smart constructor.
createDeployment :: Text -> CreateDeployment
createDeployment pApplicationName_ =
    CreateDeployment'
    { _cdrqDeploymentConfigName = Nothing
    , _cdrqRevision = Nothing
    , _cdrqDescription = Nothing
    , _cdrqIgnoreApplicationStopFailures = Nothing
    , _cdrqDeploymentGroupName = Nothing
    , _cdrqApplicationName = pApplicationName_
    }

-- | The name of an existing deployment configuration associated with the
-- applicable IAM user or AWS account.
--
-- If not specified, the value configured in the deployment group will be
-- used as the default. If the deployment group does not have a deployment
-- configuration associated with it, then CodeDeployDefault.OneAtATime will
-- be used by default.
cdrqDeploymentConfigName :: Lens' CreateDeployment (Maybe Text)
cdrqDeploymentConfigName = lens _cdrqDeploymentConfigName (\ s a -> s{_cdrqDeploymentConfigName = a});

-- | The type of revision to deploy, along with information about the
-- revision\'s location.
cdrqRevision :: Lens' CreateDeployment (Maybe RevisionLocation)
cdrqRevision = lens _cdrqRevision (\ s a -> s{_cdrqRevision = a});

-- | A comment about the deployment.
cdrqDescription :: Lens' CreateDeployment (Maybe Text)
cdrqDescription = lens _cdrqDescription (\ s a -> s{_cdrqDescription = a});

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
cdrqIgnoreApplicationStopFailures :: Lens' CreateDeployment (Maybe Bool)
cdrqIgnoreApplicationStopFailures = lens _cdrqIgnoreApplicationStopFailures (\ s a -> s{_cdrqIgnoreApplicationStopFailures = a});

-- | The deployment group\'s name.
cdrqDeploymentGroupName :: Lens' CreateDeployment (Maybe Text)
cdrqDeploymentGroupName = lens _cdrqDeploymentGroupName (\ s a -> s{_cdrqDeploymentGroupName = a});

-- | The name of an existing AWS CodeDeploy application associated with the
-- applicable IAM user or AWS account.
cdrqApplicationName :: Lens' CreateDeployment Text
cdrqApplicationName = lens _cdrqApplicationName (\ s a -> s{_cdrqApplicationName = a});

instance AWSRequest CreateDeployment where
        type Sv CreateDeployment = CodeDeploy
        type Rs CreateDeployment = CreateDeploymentResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 CreateDeploymentResponse' <$>
                   (x .?> "deploymentId") <*> (pure (fromEnum s)))

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
              ["deploymentConfigName" .= _cdrqDeploymentConfigName,
               "revision" .= _cdrqRevision,
               "description" .= _cdrqDescription,
               "ignoreApplicationStopFailures" .=
                 _cdrqIgnoreApplicationStopFailures,
               "deploymentGroupName" .= _cdrqDeploymentGroupName,
               "applicationName" .= _cdrqApplicationName]

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
-- * 'cdrsDeploymentId'
--
-- * 'cdrsStatus'
data CreateDeploymentResponse = CreateDeploymentResponse'
    { _cdrsDeploymentId :: !(Maybe Text)
    , _cdrsStatus       :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateDeploymentResponse' smart constructor.
createDeploymentResponse :: Int -> CreateDeploymentResponse
createDeploymentResponse pStatus_ =
    CreateDeploymentResponse'
    { _cdrsDeploymentId = Nothing
    , _cdrsStatus = pStatus_
    }

-- | A unique deployment ID.
cdrsDeploymentId :: Lens' CreateDeploymentResponse (Maybe Text)
cdrsDeploymentId = lens _cdrsDeploymentId (\ s a -> s{_cdrsDeploymentId = a});

-- | FIXME: Undocumented member.
cdrsStatus :: Lens' CreateDeploymentResponse Int
cdrsStatus = lens _cdrsStatus (\ s a -> s{_cdrsStatus = a});
