{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.CreateDeployment
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Runs deployment or stack commands. For more information, see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/workingapps-deploying.html Deploying Apps>
-- and
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-commands.html Run Stack Commands>.
--
-- __Required Permissions__: To use this action, an IAM user must have a
-- Deploy or Manage permissions level for the stack, or an attached policy
-- that explicitly grants permissions. For more information on user
-- permissions, see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
--
-- <http://docs.aws.amazon.com/opsworks/latest/APIReference/API_CreateDeployment.html>
module Network.AWS.OpsWorks.CreateDeployment
    (
    -- * Request
      CreateDeployment
    -- ** Request constructor
    , createDeployment
    -- ** Request lenses
    , cdrqCustomJSON
    , cdrqAppId
    , cdrqInstanceIds
    , cdrqComment
    , cdrqStackId
    , cdrqCommand

    -- * Response
    , CreateDeploymentResponse
    -- ** Response constructor
    , createDeploymentResponse
    -- ** Response lenses
    , cdrsDeploymentId
    , cdrsStatus
    ) where

import           Network.AWS.OpsWorks.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'createDeployment' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cdrqCustomJSON'
--
-- * 'cdrqAppId'
--
-- * 'cdrqInstanceIds'
--
-- * 'cdrqComment'
--
-- * 'cdrqStackId'
--
-- * 'cdrqCommand'
data CreateDeployment = CreateDeployment'
    { _cdrqCustomJSON  :: !(Maybe Text)
    , _cdrqAppId       :: !(Maybe Text)
    , _cdrqInstanceIds :: !(Maybe [Text])
    , _cdrqComment     :: !(Maybe Text)
    , _cdrqStackId     :: !Text
    , _cdrqCommand     :: !DeploymentCommand
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateDeployment' smart constructor.
createDeployment :: Text -> DeploymentCommand -> CreateDeployment
createDeployment pStackId pCommand =
    CreateDeployment'
    { _cdrqCustomJSON = Nothing
    , _cdrqAppId = Nothing
    , _cdrqInstanceIds = Nothing
    , _cdrqComment = Nothing
    , _cdrqStackId = pStackId
    , _cdrqCommand = pCommand
    }

-- | A string that contains user-defined, custom JSON. It is used to override
-- the corresponding default stack configuration JSON values. The string
-- should be in the following format and must escape characters such as
-- \'\"\':
--
-- @\"{\\\"key1\\\": \\\"value1\\\", \\\"key2\\\": \\\"value2\\\",...}\"@
--
-- For more information on custom JSON, see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-json.html Use Custom JSON to Modify the Stack Configuration Attributes>.
cdrqCustomJSON :: Lens' CreateDeployment (Maybe Text)
cdrqCustomJSON = lens _cdrqCustomJSON (\ s a -> s{_cdrqCustomJSON = a});

-- | The app ID. This parameter is required for app deployments, but not for
-- other deployment commands.
cdrqAppId :: Lens' CreateDeployment (Maybe Text)
cdrqAppId = lens _cdrqAppId (\ s a -> s{_cdrqAppId = a});

-- | The instance IDs for the deployment targets.
cdrqInstanceIds :: Lens' CreateDeployment [Text]
cdrqInstanceIds = lens _cdrqInstanceIds (\ s a -> s{_cdrqInstanceIds = a}) . _Default;

-- | A user-defined comment.
cdrqComment :: Lens' CreateDeployment (Maybe Text)
cdrqComment = lens _cdrqComment (\ s a -> s{_cdrqComment = a});

-- | The stack ID.
cdrqStackId :: Lens' CreateDeployment Text
cdrqStackId = lens _cdrqStackId (\ s a -> s{_cdrqStackId = a});

-- | A @DeploymentCommand@ object that specifies the deployment command and
-- any associated arguments.
cdrqCommand :: Lens' CreateDeployment DeploymentCommand
cdrqCommand = lens _cdrqCommand (\ s a -> s{_cdrqCommand = a});

instance AWSRequest CreateDeployment where
        type Sv CreateDeployment = OpsWorks
        type Rs CreateDeployment = CreateDeploymentResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 CreateDeploymentResponse' <$>
                   (x .?> "DeploymentId") <*> (pure (fromEnum s)))

instance ToHeaders CreateDeployment where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("OpsWorks_20130218.CreateDeployment" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateDeployment where
        toJSON CreateDeployment'{..}
          = object
              ["CustomJson" .= _cdrqCustomJSON,
               "AppId" .= _cdrqAppId,
               "InstanceIds" .= _cdrqInstanceIds,
               "Comment" .= _cdrqComment, "StackId" .= _cdrqStackId,
               "Command" .= _cdrqCommand]

instance ToPath CreateDeployment where
        toPath = const "/"

instance ToQuery CreateDeployment where
        toQuery = const mempty

-- | Contains the response to a @CreateDeployment@ request.
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
createDeploymentResponse pStatus =
    CreateDeploymentResponse'
    { _cdrsDeploymentId = Nothing
    , _cdrsStatus = pStatus
    }

-- | The deployment ID, which can be used with other requests to identify the
-- deployment.
cdrsDeploymentId :: Lens' CreateDeploymentResponse (Maybe Text)
cdrsDeploymentId = lens _cdrsDeploymentId (\ s a -> s{_cdrsDeploymentId = a});

-- | FIXME: Undocumented member.
cdrsStatus :: Lens' CreateDeploymentResponse Int
cdrsStatus = lens _cdrsStatus (\ s a -> s{_cdrsStatus = a});
