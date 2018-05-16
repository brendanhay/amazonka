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
-- Module      : Network.AWS.OpsWorks.CreateDeployment
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Runs deployment or stack commands. For more information, see <http://docs.aws.amazon.com/opsworks/latest/userguide/workingapps-deploying.html Deploying Apps> and <http://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-commands.html Run Stack Commands> .
--
--
-- __Required Permissions__ : To use this action, an IAM user must have a Deploy or Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
--
module Network.AWS.OpsWorks.CreateDeployment
    (
    -- * Creating a Request
      createDeployment
    , CreateDeployment
    -- * Request Lenses
    , cdCustomJSON
    , cdAppId
    , cdInstanceIds
    , cdLayerIds
    , cdComment
    , cdStackId
    , cdCommand

    -- * Destructuring the Response
    , createDeploymentResponse
    , CreateDeploymentResponse
    -- * Response Lenses
    , cdrsDeploymentId
    , cdrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.OpsWorks.Types
import Network.AWS.OpsWorks.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createDeployment' smart constructor.
data CreateDeployment = CreateDeployment'
  { _cdCustomJSON  :: !(Maybe Text)
  , _cdAppId       :: !(Maybe Text)
  , _cdInstanceIds :: !(Maybe [Text])
  , _cdLayerIds    :: !(Maybe [Text])
  , _cdComment     :: !(Maybe Text)
  , _cdStackId     :: !Text
  , _cdCommand     :: !DeploymentCommand
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateDeployment' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdCustomJSON' - A string that contains user-defined, custom JSON. It is used to override the corresponding default stack configuration JSON values. The string should be in the following format: @"{\"key1\": \"value1\", \"key2\": \"value2\",...}"@  For more information on custom JSON, see <http://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-json.html Use Custom JSON to Modify the Stack Configuration Attributes> .
--
-- * 'cdAppId' - The app ID. This parameter is required for app deployments, but not for other deployment commands.
--
-- * 'cdInstanceIds' - The instance IDs for the deployment targets.
--
-- * 'cdLayerIds' - The layer IDs for the deployment targets.
--
-- * 'cdComment' - A user-defined comment.
--
-- * 'cdStackId' - The stack ID.
--
-- * 'cdCommand' - A @DeploymentCommand@ object that specifies the deployment command and any associated arguments.
createDeployment
    :: Text -- ^ 'cdStackId'
    -> DeploymentCommand -- ^ 'cdCommand'
    -> CreateDeployment
createDeployment pStackId_ pCommand_ =
  CreateDeployment'
    { _cdCustomJSON = Nothing
    , _cdAppId = Nothing
    , _cdInstanceIds = Nothing
    , _cdLayerIds = Nothing
    , _cdComment = Nothing
    , _cdStackId = pStackId_
    , _cdCommand = pCommand_
    }


-- | A string that contains user-defined, custom JSON. It is used to override the corresponding default stack configuration JSON values. The string should be in the following format: @"{\"key1\": \"value1\", \"key2\": \"value2\",...}"@  For more information on custom JSON, see <http://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-json.html Use Custom JSON to Modify the Stack Configuration Attributes> .
cdCustomJSON :: Lens' CreateDeployment (Maybe Text)
cdCustomJSON = lens _cdCustomJSON (\ s a -> s{_cdCustomJSON = a})

-- | The app ID. This parameter is required for app deployments, but not for other deployment commands.
cdAppId :: Lens' CreateDeployment (Maybe Text)
cdAppId = lens _cdAppId (\ s a -> s{_cdAppId = a})

-- | The instance IDs for the deployment targets.
cdInstanceIds :: Lens' CreateDeployment [Text]
cdInstanceIds = lens _cdInstanceIds (\ s a -> s{_cdInstanceIds = a}) . _Default . _Coerce

-- | The layer IDs for the deployment targets.
cdLayerIds :: Lens' CreateDeployment [Text]
cdLayerIds = lens _cdLayerIds (\ s a -> s{_cdLayerIds = a}) . _Default . _Coerce

-- | A user-defined comment.
cdComment :: Lens' CreateDeployment (Maybe Text)
cdComment = lens _cdComment (\ s a -> s{_cdComment = a})

-- | The stack ID.
cdStackId :: Lens' CreateDeployment Text
cdStackId = lens _cdStackId (\ s a -> s{_cdStackId = a})

-- | A @DeploymentCommand@ object that specifies the deployment command and any associated arguments.
cdCommand :: Lens' CreateDeployment DeploymentCommand
cdCommand = lens _cdCommand (\ s a -> s{_cdCommand = a})

instance AWSRequest CreateDeployment where
        type Rs CreateDeployment = CreateDeploymentResponse
        request = postJSON opsWorks
        response
          = receiveJSON
              (\ s h x ->
                 CreateDeploymentResponse' <$>
                   (x .?> "DeploymentId") <*> (pure (fromEnum s)))

instance Hashable CreateDeployment where

instance NFData CreateDeployment where

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
              (catMaybes
                 [("CustomJson" .=) <$> _cdCustomJSON,
                  ("AppId" .=) <$> _cdAppId,
                  ("InstanceIds" .=) <$> _cdInstanceIds,
                  ("LayerIds" .=) <$> _cdLayerIds,
                  ("Comment" .=) <$> _cdComment,
                  Just ("StackId" .= _cdStackId),
                  Just ("Command" .= _cdCommand)])

instance ToPath CreateDeployment where
        toPath = const "/"

instance ToQuery CreateDeployment where
        toQuery = const mempty

-- | Contains the response to a @CreateDeployment@ request.
--
--
--
-- /See:/ 'createDeploymentResponse' smart constructor.
data CreateDeploymentResponse = CreateDeploymentResponse'
  { _cdrsDeploymentId   :: !(Maybe Text)
  , _cdrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateDeploymentResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdrsDeploymentId' - The deployment ID, which can be used with other requests to identify the deployment.
--
-- * 'cdrsResponseStatus' - -- | The response status code.
createDeploymentResponse
    :: Int -- ^ 'cdrsResponseStatus'
    -> CreateDeploymentResponse
createDeploymentResponse pResponseStatus_ =
  CreateDeploymentResponse'
    {_cdrsDeploymentId = Nothing, _cdrsResponseStatus = pResponseStatus_}


-- | The deployment ID, which can be used with other requests to identify the deployment.
cdrsDeploymentId :: Lens' CreateDeploymentResponse (Maybe Text)
cdrsDeploymentId = lens _cdrsDeploymentId (\ s a -> s{_cdrsDeploymentId = a})

-- | -- | The response status code.
cdrsResponseStatus :: Lens' CreateDeploymentResponse Int
cdrsResponseStatus = lens _cdrsResponseStatus (\ s a -> s{_cdrsResponseStatus = a})

instance NFData CreateDeploymentResponse where
