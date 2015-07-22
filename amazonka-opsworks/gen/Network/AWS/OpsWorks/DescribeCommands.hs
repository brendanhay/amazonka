{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.DescribeCommands
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
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
-- <http://docs.aws.amazon.com/opsworks/latest/APIReference/API_DescribeCommands.html>
module Network.AWS.OpsWorks.DescribeCommands
    (
    -- * Request
      DescribeCommands
    -- ** Request constructor
    , describeCommands
    -- ** Request lenses
    , dcrqInstanceId
    , dcrqDeploymentId
    , dcrqCommandIds

    -- * Response
    , DescribeCommandsResponse
    -- ** Response constructor
    , describeCommandsResponse
    -- ** Response lenses
    , dcrsCommands
    , dcrsStatus
    ) where

import           Network.AWS.OpsWorks.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeCommands' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcrqInstanceId'
--
-- * 'dcrqDeploymentId'
--
-- * 'dcrqCommandIds'
data DescribeCommands = DescribeCommands'
    { _dcrqInstanceId   :: !(Maybe Text)
    , _dcrqDeploymentId :: !(Maybe Text)
    , _dcrqCommandIds   :: !(Maybe [Text])
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeCommands' smart constructor.
describeCommands :: DescribeCommands
describeCommands =
    DescribeCommands'
    { _dcrqInstanceId = Nothing
    , _dcrqDeploymentId = Nothing
    , _dcrqCommandIds = Nothing
    }

-- | The instance ID. If you include this parameter, @DescribeCommands@
-- returns a description of the commands associated with the specified
-- instance.
dcrqInstanceId :: Lens' DescribeCommands (Maybe Text)
dcrqInstanceId = lens _dcrqInstanceId (\ s a -> s{_dcrqInstanceId = a});

-- | The deployment ID. If you include this parameter, @DescribeCommands@
-- returns a description of the commands associated with the specified
-- deployment.
dcrqDeploymentId :: Lens' DescribeCommands (Maybe Text)
dcrqDeploymentId = lens _dcrqDeploymentId (\ s a -> s{_dcrqDeploymentId = a});

-- | An array of command IDs. If you include this parameter,
-- @DescribeCommands@ returns a description of the specified commands.
-- Otherwise, it returns a description of every command.
dcrqCommandIds :: Lens' DescribeCommands [Text]
dcrqCommandIds = lens _dcrqCommandIds (\ s a -> s{_dcrqCommandIds = a}) . _Default;

instance AWSRequest DescribeCommands where
        type Sv DescribeCommands = OpsWorks
        type Rs DescribeCommands = DescribeCommandsResponse
        request = postJSON
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
              ["InstanceId" .= _dcrqInstanceId,
               "DeploymentId" .= _dcrqDeploymentId,
               "CommandIds" .= _dcrqCommandIds]

instance ToPath DescribeCommands where
        toPath = const "/"

instance ToQuery DescribeCommands where
        toQuery = const mempty

-- | Contains the response to a @DescribeCommands@ request.
--
-- /See:/ 'describeCommandsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcrsCommands'
--
-- * 'dcrsStatus'
data DescribeCommandsResponse = DescribeCommandsResponse'
    { _dcrsCommands :: !(Maybe [Command])
    , _dcrsStatus   :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeCommandsResponse' smart constructor.
describeCommandsResponse :: Int -> DescribeCommandsResponse
describeCommandsResponse pStatus =
    DescribeCommandsResponse'
    { _dcrsCommands = Nothing
    , _dcrsStatus = pStatus
    }

-- | An array of @Command@ objects that describe each of the specified
-- commands.
dcrsCommands :: Lens' DescribeCommandsResponse [Command]
dcrsCommands = lens _dcrsCommands (\ s a -> s{_dcrsCommands = a}) . _Default;

-- | FIXME: Undocumented member.
dcrsStatus :: Lens' DescribeCommandsResponse Int
dcrsStatus = lens _dcrsStatus (\ s a -> s{_dcrsStatus = a});
