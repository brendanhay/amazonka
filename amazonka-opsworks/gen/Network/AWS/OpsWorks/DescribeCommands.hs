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
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the results of specified commands.
--
-- You must specify at least one of the parameters.
--
-- __Required Permissions__: To use this action, an IAM user must have a
-- Show, Deploy, or Manage permissions level for the stack, or an attached
-- policy that explicitly grants permissions. For more information on user
-- permissions, see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
--
-- /See:/ <http://docs.aws.amazon.com/opsworks/latest/APIReference/API_DescribeCommands.html AWS API Reference> for DescribeCommands.
module Network.AWS.OpsWorks.DescribeCommands
    (
    -- * Creating a Request
      describeCommands
    , DescribeCommands
    -- * Request Lenses
    , dcInstanceId
    , dcDeploymentId
    , dcCommandIds

    -- * Destructuring the Response
    , describeCommandsResponse
    , DescribeCommandsResponse
    -- * Response Lenses
    , dcrsCommands
    , dcrsStatus
    ) where

import           Network.AWS.OpsWorks.Types
import           Network.AWS.OpsWorks.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeCommands' smart constructor.
data DescribeCommands = DescribeCommands'
    { _dcInstanceId   :: !(Maybe Text)
    , _dcDeploymentId :: !(Maybe Text)
    , _dcCommandIds   :: !(Maybe [Text])
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeCommands' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcInstanceId'
--
-- * 'dcDeploymentId'
--
-- * 'dcCommandIds'
describeCommands
    :: DescribeCommands
describeCommands =
    DescribeCommands'
    { _dcInstanceId = Nothing
    , _dcDeploymentId = Nothing
    , _dcCommandIds = Nothing
    }

-- | The instance ID. If you include this parameter, 'DescribeCommands'
-- returns a description of the commands associated with the specified
-- instance.
dcInstanceId :: Lens' DescribeCommands (Maybe Text)
dcInstanceId = lens _dcInstanceId (\ s a -> s{_dcInstanceId = a});

-- | The deployment ID. If you include this parameter, 'DescribeCommands'
-- returns a description of the commands associated with the specified
-- deployment.
dcDeploymentId :: Lens' DescribeCommands (Maybe Text)
dcDeploymentId = lens _dcDeploymentId (\ s a -> s{_dcDeploymentId = a});

-- | An array of command IDs. If you include this parameter,
-- 'DescribeCommands' returns a description of the specified commands.
-- Otherwise, it returns a description of every command.
dcCommandIds :: Lens' DescribeCommands [Text]
dcCommandIds = lens _dcCommandIds (\ s a -> s{_dcCommandIds = a}) . _Default . _Coerce;

instance AWSRequest DescribeCommands where
        type Rs DescribeCommands = DescribeCommandsResponse
        request = postJSON opsWorks
        response
          = receiveJSON
              (\ s h x ->
                 DescribeCommandsResponse' <$>
                   (x .?> "Commands" .!@ mempty) <*>
                     (pure (fromEnum s)))

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
                 [("InstanceId" .=) <$> _dcInstanceId,
                  ("DeploymentId" .=) <$> _dcDeploymentId,
                  ("CommandIds" .=) <$> _dcCommandIds])

instance ToPath DescribeCommands where
        toPath = const "/"

instance ToQuery DescribeCommands where
        toQuery = const mempty

-- | Contains the response to a 'DescribeCommands' request.
--
-- /See:/ 'describeCommandsResponse' smart constructor.
data DescribeCommandsResponse = DescribeCommandsResponse'
    { _dcrsCommands :: !(Maybe [Command])
    , _dcrsStatus   :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeCommandsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcrsCommands'
--
-- * 'dcrsStatus'
describeCommandsResponse
    :: Int -- ^ 'dcrsStatus'
    -> DescribeCommandsResponse
describeCommandsResponse pStatus_ =
    DescribeCommandsResponse'
    { _dcrsCommands = Nothing
    , _dcrsStatus = pStatus_
    }

-- | An array of 'Command' objects that describe each of the specified
-- commands.
dcrsCommands :: Lens' DescribeCommandsResponse [Command]
dcrsCommands = lens _dcrsCommands (\ s a -> s{_dcrsCommands = a}) . _Default . _Coerce;

-- | The response status code.
dcrsStatus :: Lens' DescribeCommandsResponse Int
dcrsStatus = lens _dcrsStatus (\ s a -> s{_dcrsStatus = a});
