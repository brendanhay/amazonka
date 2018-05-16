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
-- Module      : Network.AWS.OpsWorks.DescribeCommands
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the results of specified commands.
--
--
-- __Required Permissions__ : To use this action, an IAM user must have a Show, Deploy, or Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
--
module Network.AWS.OpsWorks.DescribeCommands
    (
    -- * Creating a Request
      describeCommands
    , DescribeCommands
    -- * Request Lenses
    , dcDeploymentId
    , dcInstanceId
    , dcCommandIds

    -- * Destructuring the Response
    , describeCommandsResponse
    , DescribeCommandsResponse
    -- * Response Lenses
    , dcrsCommands
    , dcrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.OpsWorks.Types
import Network.AWS.OpsWorks.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeCommands' smart constructor.
data DescribeCommands = DescribeCommands'
  { _dcDeploymentId :: !(Maybe Text)
  , _dcInstanceId   :: !(Maybe Text)
  , _dcCommandIds   :: !(Maybe [Text])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeCommands' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcDeploymentId' - The deployment ID. If you include this parameter, @DescribeCommands@ returns a description of the commands associated with the specified deployment.
--
-- * 'dcInstanceId' - The instance ID. If you include this parameter, @DescribeCommands@ returns a description of the commands associated with the specified instance.
--
-- * 'dcCommandIds' - An array of command IDs. If you include this parameter, @DescribeCommands@ returns a description of the specified commands. Otherwise, it returns a description of every command.
describeCommands
    :: DescribeCommands
describeCommands =
  DescribeCommands'
    { _dcDeploymentId = Nothing
    , _dcInstanceId = Nothing
    , _dcCommandIds = Nothing
    }


-- | The deployment ID. If you include this parameter, @DescribeCommands@ returns a description of the commands associated with the specified deployment.
dcDeploymentId :: Lens' DescribeCommands (Maybe Text)
dcDeploymentId = lens _dcDeploymentId (\ s a -> s{_dcDeploymentId = a})

-- | The instance ID. If you include this parameter, @DescribeCommands@ returns a description of the commands associated with the specified instance.
dcInstanceId :: Lens' DescribeCommands (Maybe Text)
dcInstanceId = lens _dcInstanceId (\ s a -> s{_dcInstanceId = a})

-- | An array of command IDs. If you include this parameter, @DescribeCommands@ returns a description of the specified commands. Otherwise, it returns a description of every command.
dcCommandIds :: Lens' DescribeCommands [Text]
dcCommandIds = lens _dcCommandIds (\ s a -> s{_dcCommandIds = a}) . _Default . _Coerce

instance AWSRequest DescribeCommands where
        type Rs DescribeCommands = DescribeCommandsResponse
        request = postJSON opsWorks
        response
          = receiveJSON
              (\ s h x ->
                 DescribeCommandsResponse' <$>
                   (x .?> "Commands" .!@ mempty) <*>
                     (pure (fromEnum s)))

instance Hashable DescribeCommands where

instance NFData DescribeCommands where

instance ToHeaders DescribeCommands where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("OpsWorks_20130218.DescribeCommands" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeCommands where
        toJSON DescribeCommands'{..}
          = object
              (catMaybes
                 [("DeploymentId" .=) <$> _dcDeploymentId,
                  ("InstanceId" .=) <$> _dcInstanceId,
                  ("CommandIds" .=) <$> _dcCommandIds])

instance ToPath DescribeCommands where
        toPath = const "/"

instance ToQuery DescribeCommands where
        toQuery = const mempty

-- | Contains the response to a @DescribeCommands@ request.
--
--
--
-- /See:/ 'describeCommandsResponse' smart constructor.
data DescribeCommandsResponse = DescribeCommandsResponse'
  { _dcrsCommands       :: !(Maybe [Command])
  , _dcrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeCommandsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcrsCommands' - An array of @Command@ objects that describe each of the specified commands.
--
-- * 'dcrsResponseStatus' - -- | The response status code.
describeCommandsResponse
    :: Int -- ^ 'dcrsResponseStatus'
    -> DescribeCommandsResponse
describeCommandsResponse pResponseStatus_ =
  DescribeCommandsResponse'
    {_dcrsCommands = Nothing, _dcrsResponseStatus = pResponseStatus_}


-- | An array of @Command@ objects that describe each of the specified commands.
dcrsCommands :: Lens' DescribeCommandsResponse [Command]
dcrsCommands = lens _dcrsCommands (\ s a -> s{_dcrsCommands = a}) . _Default . _Coerce

-- | -- | The response status code.
dcrsResponseStatus :: Lens' DescribeCommandsResponse Int
dcrsResponseStatus = lens _dcrsResponseStatus (\ s a -> s{_dcrsResponseStatus = a})

instance NFData DescribeCommandsResponse where
