{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.GameServerGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.GameServerGroup where

import Network.AWS.GameLift.Types.BalancingStrategy
import Network.AWS.GameLift.Types.GameServerGroupAction
import Network.AWS.GameLift.Types.GameServerGroupStatus
import Network.AWS.GameLift.Types.GameServerProtectionPolicy
import Network.AWS.GameLift.Types.InstanceDefinition
import Network.AWS.Lens
import Network.AWS.Prelude

-- | __This data type is used with the Amazon GameLift FleetIQ and game server groups.__
--
--
-- Properties that describe a game server group resource. A game server group manages certain properties related to a corresponding EC2 Auto Scaling group.
--
-- A game server group is created by a successful call to @CreateGameServerGroup@ and deleted by calling @DeleteGameServerGroup@ . Game server group activity can be temporarily suspended and resumed by calling @SuspendGameServerGroup@ and @ResumeGameServerGroup@ , respectively.
--
--     * 'CreateGameServerGroup'
--
--     * 'ListGameServerGroups'
--
--     * 'DescribeGameServerGroup'
--
--     * 'UpdateGameServerGroup'
--
--     * 'DeleteGameServerGroup'
--
--     * 'ResumeGameServerGroup'
--
--     * 'SuspendGameServerGroup'
--
--     * 'DescribeGameServerInstances'
--
--
--
--
-- /See:/ 'gameServerGroup' smart constructor.
data GameServerGroup = GameServerGroup'
  { _gsgCreationTime ::
      !(Maybe POSIX),
    _gsgStatus :: !(Maybe GameServerGroupStatus),
    _gsgInstanceDefinitions ::
      !(Maybe (List1 InstanceDefinition)),
    _gsgLastUpdatedTime :: !(Maybe POSIX),
    _gsgBalancingStrategy :: !(Maybe BalancingStrategy),
    _gsgGameServerGroupName :: !(Maybe Text),
    _gsgSuspendedActions ::
      !(Maybe (List1 GameServerGroupAction)),
    _gsgAutoScalingGroupARN :: !(Maybe Text),
    _gsgStatusReason :: !(Maybe Text),
    _gsgGameServerProtectionPolicy ::
      !(Maybe GameServerProtectionPolicy),
    _gsgGameServerGroupARN :: !(Maybe Text),
    _gsgRoleARN :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GameServerGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gsgCreationTime' - A timestamp that indicates when this data object was created. Format is a number expressed in Unix time as milliseconds (for example @"1469498468.057"@ ).
--
-- * 'gsgStatus' - The current status of the game server group. Possible statuses include:     * @NEW@ - GameLift FleetIQ has validated the @CreateGameServerGroup()@ request.      * @ACTIVATING@ - GameLift FleetIQ is setting up a game server group, which includes creating an Auto Scaling group in your AWS account.      * @ACTIVE@ - The game server group has been successfully created.      * @DELETE_SCHEDULED@ - A request to delete the game server group has been received.      * @DELETING@ - GameLift FleetIQ has received a valid @DeleteGameServerGroup()@ request and is processing it. GameLift FleetIQ must first complete and release hosts before it deletes the Auto Scaling group and the game server group.      * @DELETED@ - The game server group has been successfully deleted.      * @ERROR@ - The asynchronous processes of activating or deleting a game server group has failed, resulting in an error state.
--
-- * 'gsgInstanceDefinitions' - The set of EC2 instance types that GameLift FleetIQ can use when balancing and automatically scaling instances in the corresponding Auto Scaling group.
--
-- * 'gsgLastUpdatedTime' - A timestamp that indicates when this game server group was last updated.
--
-- * 'gsgBalancingStrategy' - Indicates how GameLift FleetIQ balances the use of Spot Instances and On-Demand Instances in the game server group. Method options include the following:     * @SPOT_ONLY@ - Only Spot Instances are used in the game server group. If Spot Instances are unavailable or not viable for game hosting, the game server group provides no hosting capacity until Spot Instances can again be used. Until then, no new instances are started, and the existing nonviable Spot Instances are terminated (after current gameplay ends) and are not replaced.     * @SPOT_PREFERRED@ - (default value) Spot Instances are used whenever available in the game server group. If Spot Instances are unavailable, the game server group continues to provide hosting capacity by falling back to On-Demand Instances. Existing nonviable Spot Instances are terminated (after current gameplay ends) and are replaced with new On-Demand Instances.     * @ON_DEMAND_ONLY@ - Only On-Demand Instances are used in the game server group. No Spot Instances are used, even when available, while this balancing strategy is in force.
--
-- * 'gsgGameServerGroupName' - A developer-defined identifier for the game server group. The name is unique for each Region in each AWS account.
--
-- * 'gsgSuspendedActions' - A list of activities that are currently suspended for this game server group. If this property is empty, all activities are occurring.
--
-- * 'gsgAutoScalingGroupARN' - A generated unique ID for the EC2 Auto Scaling group that is associated with this game server group.
--
-- * 'gsgStatusReason' - Additional information about the current game server group status. This information might provide additional insight on groups that are in @ERROR@ status.
--
-- * 'gsgGameServerProtectionPolicy' - A flag that indicates whether instances in the game server group are protected from early termination. Unprotected instances that have active game servers running might be terminated during a scale-down event, causing players to be dropped from the game. Protected instances cannot be terminated while there are active game servers running except in the event of a forced game server group deletion (see ). An exception to this is with Spot Instances, which can be terminated by AWS regardless of protection status.
--
-- * 'gsgGameServerGroupARN' - A generated unique ID for the game server group.
--
-- * 'gsgRoleARN' - The Amazon Resource Name (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN> ) for an IAM role that allows Amazon GameLift to access your EC2 Auto Scaling groups.
gameServerGroup ::
  GameServerGroup
gameServerGroup =
  GameServerGroup'
    { _gsgCreationTime = Nothing,
      _gsgStatus = Nothing,
      _gsgInstanceDefinitions = Nothing,
      _gsgLastUpdatedTime = Nothing,
      _gsgBalancingStrategy = Nothing,
      _gsgGameServerGroupName = Nothing,
      _gsgSuspendedActions = Nothing,
      _gsgAutoScalingGroupARN = Nothing,
      _gsgStatusReason = Nothing,
      _gsgGameServerProtectionPolicy = Nothing,
      _gsgGameServerGroupARN = Nothing,
      _gsgRoleARN = Nothing
    }

-- | A timestamp that indicates when this data object was created. Format is a number expressed in Unix time as milliseconds (for example @"1469498468.057"@ ).
gsgCreationTime :: Lens' GameServerGroup (Maybe UTCTime)
gsgCreationTime = lens _gsgCreationTime (\s a -> s {_gsgCreationTime = a}) . mapping _Time

-- | The current status of the game server group. Possible statuses include:     * @NEW@ - GameLift FleetIQ has validated the @CreateGameServerGroup()@ request.      * @ACTIVATING@ - GameLift FleetIQ is setting up a game server group, which includes creating an Auto Scaling group in your AWS account.      * @ACTIVE@ - The game server group has been successfully created.      * @DELETE_SCHEDULED@ - A request to delete the game server group has been received.      * @DELETING@ - GameLift FleetIQ has received a valid @DeleteGameServerGroup()@ request and is processing it. GameLift FleetIQ must first complete and release hosts before it deletes the Auto Scaling group and the game server group.      * @DELETED@ - The game server group has been successfully deleted.      * @ERROR@ - The asynchronous processes of activating or deleting a game server group has failed, resulting in an error state.
gsgStatus :: Lens' GameServerGroup (Maybe GameServerGroupStatus)
gsgStatus = lens _gsgStatus (\s a -> s {_gsgStatus = a})

-- | The set of EC2 instance types that GameLift FleetIQ can use when balancing and automatically scaling instances in the corresponding Auto Scaling group.
gsgInstanceDefinitions :: Lens' GameServerGroup (Maybe (NonEmpty InstanceDefinition))
gsgInstanceDefinitions = lens _gsgInstanceDefinitions (\s a -> s {_gsgInstanceDefinitions = a}) . mapping _List1

-- | A timestamp that indicates when this game server group was last updated.
gsgLastUpdatedTime :: Lens' GameServerGroup (Maybe UTCTime)
gsgLastUpdatedTime = lens _gsgLastUpdatedTime (\s a -> s {_gsgLastUpdatedTime = a}) . mapping _Time

-- | Indicates how GameLift FleetIQ balances the use of Spot Instances and On-Demand Instances in the game server group. Method options include the following:     * @SPOT_ONLY@ - Only Spot Instances are used in the game server group. If Spot Instances are unavailable or not viable for game hosting, the game server group provides no hosting capacity until Spot Instances can again be used. Until then, no new instances are started, and the existing nonviable Spot Instances are terminated (after current gameplay ends) and are not replaced.     * @SPOT_PREFERRED@ - (default value) Spot Instances are used whenever available in the game server group. If Spot Instances are unavailable, the game server group continues to provide hosting capacity by falling back to On-Demand Instances. Existing nonviable Spot Instances are terminated (after current gameplay ends) and are replaced with new On-Demand Instances.     * @ON_DEMAND_ONLY@ - Only On-Demand Instances are used in the game server group. No Spot Instances are used, even when available, while this balancing strategy is in force.
gsgBalancingStrategy :: Lens' GameServerGroup (Maybe BalancingStrategy)
gsgBalancingStrategy = lens _gsgBalancingStrategy (\s a -> s {_gsgBalancingStrategy = a})

-- | A developer-defined identifier for the game server group. The name is unique for each Region in each AWS account.
gsgGameServerGroupName :: Lens' GameServerGroup (Maybe Text)
gsgGameServerGroupName = lens _gsgGameServerGroupName (\s a -> s {_gsgGameServerGroupName = a})

-- | A list of activities that are currently suspended for this game server group. If this property is empty, all activities are occurring.
gsgSuspendedActions :: Lens' GameServerGroup (Maybe (NonEmpty GameServerGroupAction))
gsgSuspendedActions = lens _gsgSuspendedActions (\s a -> s {_gsgSuspendedActions = a}) . mapping _List1

-- | A generated unique ID for the EC2 Auto Scaling group that is associated with this game server group.
gsgAutoScalingGroupARN :: Lens' GameServerGroup (Maybe Text)
gsgAutoScalingGroupARN = lens _gsgAutoScalingGroupARN (\s a -> s {_gsgAutoScalingGroupARN = a})

-- | Additional information about the current game server group status. This information might provide additional insight on groups that are in @ERROR@ status.
gsgStatusReason :: Lens' GameServerGroup (Maybe Text)
gsgStatusReason = lens _gsgStatusReason (\s a -> s {_gsgStatusReason = a})

-- | A flag that indicates whether instances in the game server group are protected from early termination. Unprotected instances that have active game servers running might be terminated during a scale-down event, causing players to be dropped from the game. Protected instances cannot be terminated while there are active game servers running except in the event of a forced game server group deletion (see ). An exception to this is with Spot Instances, which can be terminated by AWS regardless of protection status.
gsgGameServerProtectionPolicy :: Lens' GameServerGroup (Maybe GameServerProtectionPolicy)
gsgGameServerProtectionPolicy = lens _gsgGameServerProtectionPolicy (\s a -> s {_gsgGameServerProtectionPolicy = a})

-- | A generated unique ID for the game server group.
gsgGameServerGroupARN :: Lens' GameServerGroup (Maybe Text)
gsgGameServerGroupARN = lens _gsgGameServerGroupARN (\s a -> s {_gsgGameServerGroupARN = a})

-- | The Amazon Resource Name (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN> ) for an IAM role that allows Amazon GameLift to access your EC2 Auto Scaling groups.
gsgRoleARN :: Lens' GameServerGroup (Maybe Text)
gsgRoleARN = lens _gsgRoleARN (\s a -> s {_gsgRoleARN = a})

instance FromJSON GameServerGroup where
  parseJSON =
    withObject
      "GameServerGroup"
      ( \x ->
          GameServerGroup'
            <$> (x .:? "CreationTime")
            <*> (x .:? "Status")
            <*> (x .:? "InstanceDefinitions")
            <*> (x .:? "LastUpdatedTime")
            <*> (x .:? "BalancingStrategy")
            <*> (x .:? "GameServerGroupName")
            <*> (x .:? "SuspendedActions")
            <*> (x .:? "AutoScalingGroupArn")
            <*> (x .:? "StatusReason")
            <*> (x .:? "GameServerProtectionPolicy")
            <*> (x .:? "GameServerGroupArn")
            <*> (x .:? "RoleArn")
      )

instance Hashable GameServerGroup

instance NFData GameServerGroup
