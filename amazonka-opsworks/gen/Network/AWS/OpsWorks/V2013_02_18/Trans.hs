{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- Module      : Network.AWS.OpsWorks.V2013_02_18.Trans
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | AWS OpsWorks provides a simple and flexible way to create and manage stacks
-- and applications. With AWS OpsWorks, you can provision AWS resources,
-- manage their configuration, deploy applications to those resources, and
-- monitor their health.
--
-- The 'State' operator variants from "Control.Lens.Setter" such as '.='
-- can be used to modify any additional request parameters before sending.
module Network.AWS.OpsWorks.V2013_02_18.Trans
    (
    -- * AssignVolume
    -- $AssignVolume
      assignVolume

    -- * AssociateElasticIp
    -- $AssociateElasticIp
    , associateElasticIp

    -- * AttachElasticLoadBalancer
    -- $AttachElasticLoadBalancer
    , attachElasticLoadBalancer

    -- * CloneStack
    -- $CloneStack
    , cloneStack

    -- * CreateApp
    -- $CreateApp
    , createApp

    -- * CreateDeployment
    -- $CreateDeployment
    , createDeployment

    -- * CreateInstance
    -- $CreateInstance
    , createInstance

    -- * CreateLayer
    -- $CreateLayer
    , createLayer

    -- * CreateStack
    -- $CreateStack
    , createStack

    -- * CreateUserProfile
    -- $CreateUserProfile
    , createUserProfile

    -- * DeleteApp
    -- $DeleteApp
    , deleteApp

    -- * DeleteInstance
    -- $DeleteInstance
    , deleteInstance

    -- * DeleteLayer
    -- $DeleteLayer
    , deleteLayer

    -- * DeleteStack
    -- $DeleteStack
    , deleteStack

    -- * DeleteUserProfile
    -- $DeleteUserProfile
    , deleteUserProfile

    -- * DeregisterElasticIp
    -- $DeregisterElasticIp
    , deregisterElasticIp

    -- * DeregisterRdsDbInstance
    -- $DeregisterRdsDbInstance
    , deregisterRdsDbInstance

    -- * DeregisterVolume
    -- $DeregisterVolume
    , deregisterVolume

    -- * DescribeApps
    -- $DescribeApps
    , describeApps

    -- * DescribeCommands
    -- $DescribeCommands
    , describeCommands

    -- * DescribeDeployments
    -- $DescribeDeployments
    , describeDeployments

    -- * DescribeElasticIps
    -- $DescribeElasticIps
    , describeElasticIps

    -- * DescribeElasticLoadBalancers
    -- $DescribeElasticLoadBalancers
    , describeElasticLoadBalancers

    -- * DescribeInstances
    -- $DescribeInstances
    , describeInstances

    -- * DescribeLayers
    -- $DescribeLayers
    , describeLayers

    -- * DescribeLoadBasedAutoScaling
    -- $DescribeLoadBasedAutoScaling
    , describeLoadBasedAutoScaling

    -- * DescribeMyUserProfile
    -- $DescribeMyUserProfile
    , describeMyUserProfile

    -- * DescribePermissions
    -- $DescribePermissions
    , describePermissions

    -- * DescribeRaidArrays
    -- $DescribeRaidArrays
    , describeRaidArrays

    -- * DescribeRdsDbInstances
    -- $DescribeRdsDbInstances
    , describeRdsDbInstances

    -- * DescribeServiceErrors
    -- $DescribeServiceErrors
    , describeServiceErrors

    -- * DescribeStackSummary
    -- $DescribeStackSummary
    , describeStackSummary

    -- * DescribeStacks
    -- $DescribeStacks
    , describeStacks

    -- * DescribeTimeBasedAutoScaling
    -- $DescribeTimeBasedAutoScaling
    , describeTimeBasedAutoScaling

    -- * DescribeUserProfiles
    -- $DescribeUserProfiles
    , describeUserProfiles

    -- * DescribeVolumes
    -- $DescribeVolumes
    , describeVolumes

    -- * DetachElasticLoadBalancer
    -- $DetachElasticLoadBalancer
    , detachElasticLoadBalancer

    -- * DisassociateElasticIp
    -- $DisassociateElasticIp
    , disassociateElasticIp

    -- * GetHostnameSuggestion
    -- $GetHostnameSuggestion
    , getHostnameSuggestion

    -- * RebootInstance
    -- $RebootInstance
    , rebootInstance

    -- * RegisterElasticIp
    -- $RegisterElasticIp
    , registerElasticIp

    -- * RegisterRdsDbInstance
    -- $RegisterRdsDbInstance
    , registerRdsDbInstance

    -- * RegisterVolume
    -- $RegisterVolume
    , registerVolume

    -- * SetLoadBasedAutoScaling
    -- $SetLoadBasedAutoScaling
    , setLoadBasedAutoScaling

    -- * SetPermission
    -- $SetPermission
    , setPermission

    -- * SetTimeBasedAutoScaling
    -- $SetTimeBasedAutoScaling
    , setTimeBasedAutoScaling

    -- * StartInstance
    -- $StartInstance
    , startInstance

    -- * StartStack
    -- $StartStack
    , startStack

    -- * StopInstance
    -- $StopInstance
    , stopInstance

    -- * StopStack
    -- $StopStack
    , stopStack

    -- * UnassignVolume
    -- $UnassignVolume
    , unassignVolume

    -- * UpdateApp
    -- $UpdateApp
    , updateApp

    -- * UpdateElasticIp
    -- $UpdateElasticIp
    , updateElasticIp

    -- * UpdateInstance
    -- $UpdateInstance
    , updateInstance

    -- * UpdateLayer
    -- $UpdateLayer
    , updateLayer

    -- * UpdateMyUserProfile
    -- $UpdateMyUserProfile
    , updateMyUserProfile

    -- * UpdateRdsDbInstance
    -- $UpdateRdsDbInstance
    , updateRdsDbInstance

    -- * UpdateStack
    -- $UpdateStack
    , updateStack

    -- * UpdateUserProfile
    -- $UpdateUserProfile
    , updateUserProfile

    -- * UpdateVolume
    -- $UpdateVolume
    , updateVolume

    -- * Re-exported
    , module AWS
    , module Network.AWS.OpsWorks.V2013_02_18
    , (.=)
    , (?=)
    , (<>=)
    , (%=)
    ) where

import Control.Monad.Trans.AWS as AWS
import Network.AWS.Prelude
import Network.AWS.OpsWorks.V2013_02_18

-- $AssignVolume
-- Assigns one of the stack's registered Amazon EBS volumes to a specified
-- instance. The volume must first be registered with the stack by calling
-- RegisterVolume. For more information, see Resource Management. Required
-- Permissions: To use this action, an IAM user must have a Manage permissions
-- level for the stack, or an attached policy that explicitly grants
-- permissions. For more information on user permissions, see Managing User
-- Permissions.
--
-- See: 'Network.AWS.OpsWorks.V2013_02_18.AssignVolume'

assignVolume :: ( MonadCatch m
                , MonadResource m
                , MonadError AWS.Error m
                , MonadReader Env m
                )
             => Text -- ^ 'avVolumeId'
             -> State AssignVolume a
             -> m AssignVolumeResponse
assignVolume p1 s =
    send $ (mkAssignVolume p1) &~ s

-- $AssociateElasticIp
-- Associates one of the stack's registered Elastic IP addresses with a
-- specified instance. The address must first be registered with the stack by
-- calling RegisterElasticIp. For more information, see Resource Management.
-- Required Permissions: To use this action, an IAM user must have a Manage
-- permissions level for the stack, or an attached policy that explicitly
-- grants permissions. For more information on user permissions, see Managing
-- User Permissions.
--
-- See: 'Network.AWS.OpsWorks.V2013_02_18.AssociateElasticIp'

associateElasticIp :: ( MonadCatch m
                      , MonadResource m
                      , MonadError AWS.Error m
                      , MonadReader Env m
                      )
                   => Text -- ^ 'aeiElasticIp'
                   -> State AssociateElasticIp a
                   -> m AssociateElasticIpResponse
associateElasticIp p1 s =
    send $ (mkAssociateElasticIp p1) &~ s

-- $AttachElasticLoadBalancer
-- Attaches an Elastic Load Balancing load balancer to a specified layer. For
-- more information, see Elastic Load Balancing. You must create the Elastic
-- Load Balancing instance separately, by using the Elastic Load Balancing
-- console, API, or CLI. For more information, see Elastic Load Balancing
-- Developer Guide. Required Permissions: To use this action, an IAM user must
-- have a Manage permissions level for the stack, or an attached policy that
-- explicitly grants permissions. For more information on user permissions,
-- see Managing User Permissions.
--
-- See: 'Network.AWS.OpsWorks.V2013_02_18.AttachElasticLoadBalancer'

attachElasticLoadBalancer :: ( MonadCatch m
                             , MonadResource m
                             , MonadError AWS.Error m
                             , MonadReader Env m
                             )
                          => Text -- ^ 'aelbElasticLoadBalancerName'
                          -> Text -- ^ 'aelbLayerId'
                          -> State AttachElasticLoadBalancer a
                          -> m AttachElasticLoadBalancerResponse
attachElasticLoadBalancer p1 p2 s =
    send $ (mkAttachElasticLoadBalancer p1 p2) &~ s

-- $CloneStack
-- Creates a clone of a specified stack. For more information, see Clone a
-- Stack. Required Permissions: To use this action, an IAM user must have an
-- attached policy that explicitly grants permissions. For more information on
-- user permissions, see Managing User Permissions.
--
-- See: 'Network.AWS.OpsWorks.V2013_02_18.CloneStack'

cloneStack :: ( MonadCatch m
              , MonadResource m
              , MonadError AWS.Error m
              , MonadReader Env m
              )
           => Text -- ^ 'csSourceStackId'
           -> Text -- ^ 'csServiceRoleArn'
           -> State CloneStack a
           -> m CloneStackResponse
cloneStack p1 p6 s =
    send $ (mkCloneStack p1 p6) &~ s

-- $CreateApp
-- Creates an app for a specified stack. For more information, see Creating
-- Apps. Required Permissions: To use this action, an IAM user must have a
-- Manage permissions level for the stack, or an attached policy that
-- explicitly grants permissions. For more information on user permissions,
-- see Managing User Permissions.
--
-- See: 'Network.AWS.OpsWorks.V2013_02_18.CreateApp'

createApp :: ( MonadCatch m
             , MonadResource m
             , MonadError AWS.Error m
             , MonadReader Env m
             )
          => Text -- ^ 'caStackId'
          -> Text -- ^ 'caName'
          -> AppType -- ^ 'caType'
          -> State CreateApp a
          -> m CreateAppResponse
createApp p1 p3 p6 s =
    send $ (mkCreateApp p1 p3 p6) &~ s

-- $CreateDeployment
-- Deploys a stack or app. App deployment generates a deploy event, which runs
-- the associated recipes and passes them a JSON stack configuration object
-- that includes information about the app. Stack deployment runs the deploy
-- recipes but does not raise an event. For more information, see Deploying
-- Apps and Run Stack Commands. Required Permissions: To use this action, an
-- IAM user must have a Deploy or Manage permissions level for the stack, or
-- an attached policy that explicitly grants permissions. For more information
-- on user permissions, see Managing User Permissions.
--
-- See: 'Network.AWS.OpsWorks.V2013_02_18.CreateDeployment'

createDeployment :: ( MonadCatch m
                    , MonadResource m
                    , MonadError AWS.Error m
                    , MonadReader Env m
                    )
                 => Text -- ^ 'cdStackId'
                 -> DeploymentCommand -- ^ 'cdCommand'
                 -> State CreateDeployment a
                 -> m CreateDeploymentResponse
createDeployment p1 p4 s =
    send $ (mkCreateDeployment p1 p4) &~ s

-- $CreateInstance
-- Creates an instance in a specified stack. For more information, see Adding
-- an Instance to a Layer. Required Permissions: To use this action, an IAM
-- user must have a Manage permissions level for the stack, or an attached
-- policy that explicitly grants permissions. For more information on user
-- permissions, see Managing User Permissions.
--
-- See: 'Network.AWS.OpsWorks.V2013_02_18.CreateInstance'

createInstance :: ( MonadCatch m
                  , MonadResource m
                  , MonadError AWS.Error m
                  , MonadReader Env m
                  )
               => Text -- ^ 'ciStackId'
               -> [Text] -- ^ 'ciLayerIds'
               -> Text -- ^ 'ciInstanceType'
               -> State CreateInstance a
               -> m CreateInstanceResponse
createInstance p1 p2 p3 s =
    send $ (mkCreateInstance p1 p2 p3) &~ s

-- $CreateLayer
-- Creates a layer. For more information, see How to Create a Layer. You
-- should use CreateLayer for noncustom layer types such as PHP App Server
-- only if the stack does not have an existing layer of that type. A stack can
-- have at most one instance of each noncustom layer; if you attempt to create
-- a second instance, CreateLayer fails. A stack can have an arbitrary number
-- of custom layers, so you can call CreateLayer as many times as you like for
-- that layer type. Required Permissions: To use this action, an IAM user must
-- have a Manage permissions level for the stack, or an attached policy that
-- explicitly grants permissions. For more information on user permissions,
-- see Managing User Permissions.
--
-- See: 'Network.AWS.OpsWorks.V2013_02_18.CreateLayer'

createLayer :: ( MonadCatch m
               , MonadResource m
               , MonadError AWS.Error m
               , MonadReader Env m
               )
            => Text -- ^ 'clStackId'
            -> LayerType -- ^ 'clType'
            -> Text -- ^ 'clName'
            -> Text -- ^ 'clShortname'
            -> State CreateLayer a
            -> m CreateLayerResponse
createLayer p1 p2 p3 p4 s =
    send $ (mkCreateLayer p1 p2 p3 p4) &~ s

-- $CreateStack
-- Creates a new stack. For more information, see Create a New Stack. Required
-- Permissions: To use this action, an IAM user must have an attached policy
-- that explicitly grants permissions. For more information on user
-- permissions, see Managing User Permissions.
--
-- See: 'Network.AWS.OpsWorks.V2013_02_18.CreateStack'

createStack :: ( MonadCatch m
               , MonadResource m
               , MonadError AWS.Error m
               , MonadReader Env m
               )
            => Text -- ^ 'cs1Name'
            -> Text -- ^ 'cs1Region'
            -> Text -- ^ 'cs1ServiceRoleArn'
            -> Text -- ^ 'cs1DefaultInstanceProfileArn'
            -> State CreateStack a
            -> m CreateStackResponse
createStack p1 p2 p5 p6 s =
    send $ (mkCreateStack p1 p2 p5 p6) &~ s

-- $CreateUserProfile
-- Creates a new user profile. Required Permissions: To use this action, an
-- IAM user must have an attached policy that explicitly grants permissions.
-- For more information on user permissions, see Managing User Permissions.
--
-- See: 'Network.AWS.OpsWorks.V2013_02_18.CreateUserProfile'

createUserProfile :: ( MonadCatch m
                     , MonadResource m
                     , MonadError AWS.Error m
                     , MonadReader Env m
                     )
                  => Text -- ^ 'cupIamUserArn'
                  -> State CreateUserProfile a
                  -> m CreateUserProfileResponse
createUserProfile p1 s =
    send $ (mkCreateUserProfile p1) &~ s

-- $DeleteApp
-- Deletes a specified app. Required Permissions: To use this action, an IAM
-- user must have a Manage permissions level for the stack, or an attached
-- policy that explicitly grants permissions. For more information on user
-- permissions, see Managing User Permissions.
--
-- See: 'Network.AWS.OpsWorks.V2013_02_18.DeleteApp'

deleteApp :: ( MonadCatch m
             , MonadResource m
             , MonadError AWS.Error m
             , MonadReader Env m
             )
          => Text -- ^ 'daAppId'
          -> State DeleteApp a
          -> m DeleteAppResponse
deleteApp p1 s =
    send $ (mkDeleteApp p1) &~ s

-- $DeleteInstance
-- Deletes a specified instance. You must stop an instance before you can
-- delete it. For more information, see Deleting Instances. Required
-- Permissions: To use this action, an IAM user must have a Manage permissions
-- level for the stack, or an attached policy that explicitly grants
-- permissions. For more information on user permissions, see Managing User
-- Permissions.
--
-- See: 'Network.AWS.OpsWorks.V2013_02_18.DeleteInstance'

deleteInstance :: ( MonadCatch m
                  , MonadResource m
                  , MonadError AWS.Error m
                  , MonadReader Env m
                  )
               => Text -- ^ 'diInstanceId'
               -> State DeleteInstance a
               -> m DeleteInstanceResponse
deleteInstance p1 s =
    send $ (mkDeleteInstance p1) &~ s

-- $DeleteLayer
-- Deletes a specified layer. You must first stop and then delete all
-- associated instances. For more information, see How to Delete a Layer.
-- Required Permissions: To use this action, an IAM user must have a Manage
-- permissions level for the stack, or an attached policy that explicitly
-- grants permissions. For more information on user permissions, see Managing
-- User Permissions.
--
-- See: 'Network.AWS.OpsWorks.V2013_02_18.DeleteLayer'

deleteLayer :: ( MonadCatch m
               , MonadResource m
               , MonadError AWS.Error m
               , MonadReader Env m
               )
            => Text -- ^ 'dlLayerId'
            -> State DeleteLayer a
            -> m DeleteLayerResponse
deleteLayer p1 s =
    send $ (mkDeleteLayer p1) &~ s

-- $DeleteStack
-- Deletes a specified stack. You must first delete all instances, layers, and
-- apps. For more information, see Shut Down a Stack. Required Permissions: To
-- use this action, an IAM user must have a Manage permissions level for the
-- stack, or an attached policy that explicitly grants permissions. For more
-- information on user permissions, see Managing User Permissions.
--
-- See: 'Network.AWS.OpsWorks.V2013_02_18.DeleteStack'

deleteStack :: ( MonadCatch m
               , MonadResource m
               , MonadError AWS.Error m
               , MonadReader Env m
               )
            => Text -- ^ 'ds1StackId'
            -> State DeleteStack a
            -> m DeleteStackResponse
deleteStack p1 s =
    send $ (mkDeleteStack p1) &~ s

-- $DeleteUserProfile
-- Deletes a user profile. Required Permissions: To use this action, an IAM
-- user must have an attached policy that explicitly grants permissions. For
-- more information on user permissions, see Managing User Permissions.
--
-- See: 'Network.AWS.OpsWorks.V2013_02_18.DeleteUserProfile'

deleteUserProfile :: ( MonadCatch m
                     , MonadResource m
                     , MonadError AWS.Error m
                     , MonadReader Env m
                     )
                  => Text -- ^ 'dupIamUserArn'
                  -> State DeleteUserProfile a
                  -> m DeleteUserProfileResponse
deleteUserProfile p1 s =
    send $ (mkDeleteUserProfile p1) &~ s

-- $DeregisterElasticIp
-- Deregisters a specified Elastic IP address. The address can then be
-- registered by another stack. For more information, see Resource Management.
-- Required Permissions: To use this action, an IAM user must have a Manage
-- permissions level for the stack, or an attached policy that explicitly
-- grants permissions. For more information on user permissions, see Managing
-- User Permissions.
--
-- See: 'Network.AWS.OpsWorks.V2013_02_18.DeregisterElasticIp'

deregisterElasticIp :: ( MonadCatch m
                       , MonadResource m
                       , MonadError AWS.Error m
                       , MonadReader Env m
                       )
                    => Text -- ^ 'deiElasticIp'
                    -> State DeregisterElasticIp a
                    -> m DeregisterElasticIpResponse
deregisterElasticIp p1 s =
    send $ (mkDeregisterElasticIp p1) &~ s

-- $DeregisterRdsDbInstance
-- Deregisters an Amazon RDS instance.
--
-- See: 'Network.AWS.OpsWorks.V2013_02_18.DeregisterRdsDbInstance'

deregisterRdsDbInstance :: ( MonadCatch m
                           , MonadResource m
                           , MonadError AWS.Error m
                           , MonadReader Env m
                           )
                        => Text -- ^ 'drdiRdsDbInstanceArn'
                        -> State DeregisterRdsDbInstance a
                        -> m DeregisterRdsDbInstanceResponse
deregisterRdsDbInstance p1 s =
    send $ (mkDeregisterRdsDbInstance p1) &~ s

-- $DeregisterVolume
-- Deregisters an Amazon EBS volume. The volume can then be registered by
-- another stack. For more information, see Resource Management. Required
-- Permissions: To use this action, an IAM user must have a Manage permissions
-- level for the stack, or an attached policy that explicitly grants
-- permissions. For more information on user permissions, see Managing User
-- Permissions.
--
-- See: 'Network.AWS.OpsWorks.V2013_02_18.DeregisterVolume'

deregisterVolume :: ( MonadCatch m
                    , MonadResource m
                    , MonadError AWS.Error m
                    , MonadReader Env m
                    )
                 => Text -- ^ 'dvVolumeId'
                 -> State DeregisterVolume a
                 -> m DeregisterVolumeResponse
deregisterVolume p1 s =
    send $ (mkDeregisterVolume p1) &~ s

-- $DescribeApps
-- Requests a description of a specified set of apps. You must specify at
-- least one of the parameters. Required Permissions: To use this action, an
-- IAM user must have a Show, Deploy, or Manage permissions level for the
-- stack, or an attached policy that explicitly grants permissions. For more
-- information on user permissions, see Managing User Permissions.
--
-- See: 'Network.AWS.OpsWorks.V2013_02_18.DescribeApps'

describeApps :: ( MonadCatch m
                , MonadResource m
                , MonadError AWS.Error m
                , MonadReader Env m
                )
             => State DescribeApps a
             -> m DescribeAppsResponse
describeApps s =
    send (mkDescribeApps &~ s)

-- $DescribeCommands
-- Describes the results of specified commands. You must specify at least one
-- of the parameters. Required Permissions: To use this action, an IAM user
-- must have a Show, Deploy, or Manage permissions level for the stack, or an
-- attached policy that explicitly grants permissions. For more information on
-- user permissions, see Managing User Permissions.
--
-- See: 'Network.AWS.OpsWorks.V2013_02_18.DescribeCommands'

describeCommands :: ( MonadCatch m
                    , MonadResource m
                    , MonadError AWS.Error m
                    , MonadReader Env m
                    )
                 => State DescribeCommands a
                 -> m DescribeCommandsResponse
describeCommands s =
    send (mkDescribeCommands &~ s)

-- $DescribeDeployments
-- Requests a description of a specified set of deployments. You must specify
-- at least one of the parameters. Required Permissions: To use this action,
-- an IAM user must have a Show, Deploy, or Manage permissions level for the
-- stack, or an attached policy that explicitly grants permissions. For more
-- information on user permissions, see Managing User Permissions.
--
-- See: 'Network.AWS.OpsWorks.V2013_02_18.DescribeDeployments'

describeDeployments :: ( MonadCatch m
                       , MonadResource m
                       , MonadError AWS.Error m
                       , MonadReader Env m
                       )
                    => State DescribeDeployments a
                    -> m DescribeDeploymentsResponse
describeDeployments s =
    send (mkDescribeDeployments &~ s)

-- $DescribeElasticIps
-- Describes Elastic IP addresses. You must specify at least one of the
-- parameters. Required Permissions: To use this action, an IAM user must have
-- a Show, Deploy, or Manage permissions level for the stack, or an attached
-- policy that explicitly grants permissions. For more information on user
-- permissions, see Managing User Permissions.
--
-- See: 'Network.AWS.OpsWorks.V2013_02_18.DescribeElasticIps'

describeElasticIps :: ( MonadCatch m
                      , MonadResource m
                      , MonadError AWS.Error m
                      , MonadReader Env m
                      )
                   => State DescribeElasticIps a
                   -> m DescribeElasticIpsResponse
describeElasticIps s =
    send (mkDescribeElasticIps &~ s)

-- $DescribeElasticLoadBalancers
-- Describes a stack's Elastic Load Balancing instances. You must specify at
-- least one of the parameters. Required Permissions: To use this action, an
-- IAM user must have a Show, Deploy, or Manage permissions level for the
-- stack, or an attached policy that explicitly grants permissions. For more
-- information on user permissions, see Managing User Permissions.
--
-- See: 'Network.AWS.OpsWorks.V2013_02_18.DescribeElasticLoadBalancers'

describeElasticLoadBalancers :: ( MonadCatch m
                                , MonadResource m
                                , MonadError AWS.Error m
                                , MonadReader Env m
                                )
                             => State DescribeElasticLoadBalancers a
                             -> m DescribeElasticLoadBalancersResponse
describeElasticLoadBalancers s =
    send (mkDescribeElasticLoadBalancers &~ s)

-- $DescribeInstances
-- Requests a description of a set of instances. You must specify at least one
-- of the parameters. Required Permissions: To use this action, an IAM user
-- must have a Show, Deploy, or Manage permissions level for the stack, or an
-- attached policy that explicitly grants permissions. For more information on
-- user permissions, see Managing User Permissions.
--
-- See: 'Network.AWS.OpsWorks.V2013_02_18.DescribeInstances'

describeInstances :: ( MonadCatch m
                     , MonadResource m
                     , MonadError AWS.Error m
                     , MonadReader Env m
                     )
                  => State DescribeInstances a
                  -> m DescribeInstancesResponse
describeInstances s =
    send (mkDescribeInstances &~ s)

-- $DescribeLayers
-- Requests a description of one or more layers in a specified stack. You must
-- specify at least one of the parameters. Required Permissions: To use this
-- action, an IAM user must have a Show, Deploy, or Manage permissions level
-- for the stack, or an attached policy that explicitly grants permissions.
-- For more information on user permissions, see Managing User Permissions.
--
-- See: 'Network.AWS.OpsWorks.V2013_02_18.DescribeLayers'

describeLayers :: ( MonadCatch m
                  , MonadResource m
                  , MonadError AWS.Error m
                  , MonadReader Env m
                  )
               => State DescribeLayers a
               -> m DescribeLayersResponse
describeLayers s =
    send (mkDescribeLayers &~ s)

-- $DescribeLoadBasedAutoScaling
-- Describes load-based auto scaling configurations for specified layers. You
-- must specify at least one of the parameters. Required Permissions: To use
-- this action, an IAM user must have a Show, Deploy, or Manage permissions
-- level for the stack, or an attached policy that explicitly grants
-- permissions. For more information on user permissions, see Managing User
-- Permissions.
--
-- See: 'Network.AWS.OpsWorks.V2013_02_18.DescribeLoadBasedAutoScaling'

describeLoadBasedAutoScaling :: ( MonadCatch m
                                , MonadResource m
                                , MonadError AWS.Error m
                                , MonadReader Env m
                                )
                             => [Text] -- ^ 'dlbasLayerIds'
                             -> State DescribeLoadBasedAutoScaling a
                             -> m DescribeLoadBasedAutoScalingResponse
describeLoadBasedAutoScaling p1 s =
    send $ (mkDescribeLoadBasedAutoScaling p1) &~ s

-- $DescribeMyUserProfile
-- Describes a user's SSH information. Required Permissions: To use this
-- action, an IAM user must have self-management enabled or an attached policy
-- that explicitly grants permissions. For more information on user
-- permissions, see Managing User Permissions.
--
-- See: 'Network.AWS.OpsWorks.V2013_02_18.DescribeMyUserProfile'

describeMyUserProfile :: ( MonadCatch m
                         , MonadResource m
                         , MonadError AWS.Error m
                         , MonadReader Env m
                         )
                      => State DescribeMyUserProfile a
                      -> m DescribeMyUserProfileResponse
describeMyUserProfile s =
    send (mkDescribeMyUserProfile &~ s)

-- $DescribePermissions
-- Describes the permissions for a specified stack. Required Permissions: To
-- use this action, an IAM user must have a Manage permissions level for the
-- stack, or an attached policy that explicitly grants permissions. For more
-- information on user permissions, see Managing User Permissions.
--
-- See: 'Network.AWS.OpsWorks.V2013_02_18.DescribePermissions'

describePermissions :: ( MonadCatch m
                       , MonadResource m
                       , MonadError AWS.Error m
                       , MonadReader Env m
                       )
                    => State DescribePermissions a
                    -> m DescribePermissionsResponse
describePermissions s =
    send (mkDescribePermissions &~ s)

-- $DescribeRaidArrays
-- Describe an instance's RAID arrays. You must specify at least one of the
-- parameters. Required Permissions: To use this action, an IAM user must have
-- a Show, Deploy, or Manage permissions level for the stack, or an attached
-- policy that explicitly grants permissions. For more information on user
-- permissions, see Managing User Permissions.
--
-- See: 'Network.AWS.OpsWorks.V2013_02_18.DescribeRaidArrays'

describeRaidArrays :: ( MonadCatch m
                      , MonadResource m
                      , MonadError AWS.Error m
                      , MonadReader Env m
                      )
                   => State DescribeRaidArrays a
                   -> m DescribeRaidArraysResponse
describeRaidArrays s =
    send (mkDescribeRaidArrays &~ s)

-- $DescribeRdsDbInstances
-- Describes Amazon RDS instances.
--
-- See: 'Network.AWS.OpsWorks.V2013_02_18.DescribeRdsDbInstances'

describeRdsDbInstances :: ( MonadCatch m
                          , MonadResource m
                          , MonadError AWS.Error m
                          , MonadReader Env m
                          )
                       => Text -- ^ 'drdi1StackId'
                       -> State DescribeRdsDbInstances a
                       -> m DescribeRdsDbInstancesResponse
describeRdsDbInstances p1 s =
    send $ (mkDescribeRdsDbInstances p1) &~ s

-- $DescribeServiceErrors
-- Describes AWS OpsWorks service errors. Required Permissions: To use this
-- action, an IAM user must have a Show, Deploy, or Manage permissions level
-- for the stack, or an attached policy that explicitly grants permissions.
-- For more information on user permissions, see Managing User Permissions.
--
-- See: 'Network.AWS.OpsWorks.V2013_02_18.DescribeServiceErrors'

describeServiceErrors :: ( MonadCatch m
                         , MonadResource m
                         , MonadError AWS.Error m
                         , MonadReader Env m
                         )
                      => State DescribeServiceErrors a
                      -> m DescribeServiceErrorsResponse
describeServiceErrors s =
    send (mkDescribeServiceErrors &~ s)

-- $DescribeStackSummary
-- Describes the number of layers and apps in a specified stack, and the
-- number of instances in each state, such as running_setup or online.
-- Required Permissions: To use this action, an IAM user must have a Show,
-- Deploy, or Manage permissions level for the stack, or an attached policy
-- that explicitly grants permissions. For more information on user
-- permissions, see Managing User Permissions.
--
-- See: 'Network.AWS.OpsWorks.V2013_02_18.DescribeStackSummary'

describeStackSummary :: ( MonadCatch m
                        , MonadResource m
                        , MonadError AWS.Error m
                        , MonadReader Env m
                        )
                     => Text -- ^ 'dssStackId'
                     -> State DescribeStackSummary a
                     -> m DescribeStackSummaryResponse
describeStackSummary p1 s =
    send $ (mkDescribeStackSummary p1) &~ s

-- $DescribeStacks
-- Requests a description of one or more stacks. Required Permissions: To use
-- this action, an IAM user must have a Show, Deploy, or Manage permissions
-- level for the stack, or an attached policy that explicitly grants
-- permissions. For more information on user permissions, see Managing User
-- Permissions.
--
-- See: 'Network.AWS.OpsWorks.V2013_02_18.DescribeStacks'

describeStacks :: ( MonadCatch m
                  , MonadResource m
                  , MonadError AWS.Error m
                  , MonadReader Env m
                  )
               => State DescribeStacks a
               -> m DescribeStacksResponse
describeStacks s =
    send (mkDescribeStacks &~ s)

-- $DescribeTimeBasedAutoScaling
-- Describes time-based auto scaling configurations for specified instances.
-- You must specify at least one of the parameters. Required Permissions: To
-- use this action, an IAM user must have a Show, Deploy, or Manage
-- permissions level for the stack, or an attached policy that explicitly
-- grants permissions. For more information on user permissions, see Managing
-- User Permissions.
--
-- See: 'Network.AWS.OpsWorks.V2013_02_18.DescribeTimeBasedAutoScaling'

describeTimeBasedAutoScaling :: ( MonadCatch m
                                , MonadResource m
                                , MonadError AWS.Error m
                                , MonadReader Env m
                                )
                             => [Text] -- ^ 'dtbasInstanceIds'
                             -> State DescribeTimeBasedAutoScaling a
                             -> m DescribeTimeBasedAutoScalingResponse
describeTimeBasedAutoScaling p1 s =
    send $ (mkDescribeTimeBasedAutoScaling p1) &~ s

-- $DescribeUserProfiles
-- Describe specified users. Required Permissions: To use this action, an IAM
-- user must have an attached policy that explicitly grants permissions. For
-- more information on user permissions, see Managing User Permissions.
--
-- See: 'Network.AWS.OpsWorks.V2013_02_18.DescribeUserProfiles'

describeUserProfiles :: ( MonadCatch m
                        , MonadResource m
                        , MonadError AWS.Error m
                        , MonadReader Env m
                        )
                     => State DescribeUserProfiles a
                     -> m DescribeUserProfilesResponse
describeUserProfiles s =
    send (mkDescribeUserProfiles &~ s)

-- $DescribeVolumes
-- Describes an instance's Amazon EBS volumes. You must specify at least one
-- of the parameters. Required Permissions: To use this action, an IAM user
-- must have a Show, Deploy, or Manage permissions level for the stack, or an
-- attached policy that explicitly grants permissions. For more information on
-- user permissions, see Managing User Permissions.
--
-- See: 'Network.AWS.OpsWorks.V2013_02_18.DescribeVolumes'

describeVolumes :: ( MonadCatch m
                   , MonadResource m
                   , MonadError AWS.Error m
                   , MonadReader Env m
                   )
                => State DescribeVolumes a
                -> m DescribeVolumesResponse
describeVolumes s =
    send (mkDescribeVolumes &~ s)

-- $DetachElasticLoadBalancer
-- Detaches a specified Elastic Load Balancing instance from its layer.
-- Required Permissions: To use this action, an IAM user must have a Manage
-- permissions level for the stack, or an attached policy that explicitly
-- grants permissions. For more information on user permissions, see Managing
-- User Permissions.
--
-- See: 'Network.AWS.OpsWorks.V2013_02_18.DetachElasticLoadBalancer'

detachElasticLoadBalancer :: ( MonadCatch m
                             , MonadResource m
                             , MonadError AWS.Error m
                             , MonadReader Env m
                             )
                          => Text -- ^ 'delb1ElasticLoadBalancerName'
                          -> Text -- ^ 'delb1LayerId'
                          -> State DetachElasticLoadBalancer a
                          -> m DetachElasticLoadBalancerResponse
detachElasticLoadBalancer p1 p2 s =
    send $ (mkDetachElasticLoadBalancer p1 p2) &~ s

-- $DisassociateElasticIp
-- Disassociates an Elastic IP address from its instance. The address remains
-- registered with the stack. For more information, see Resource Management.
-- Required Permissions: To use this action, an IAM user must have a Manage
-- permissions level for the stack, or an attached policy that explicitly
-- grants permissions. For more information on user permissions, see Managing
-- User Permissions.
--
-- See: 'Network.AWS.OpsWorks.V2013_02_18.DisassociateElasticIp'

disassociateElasticIp :: ( MonadCatch m
                         , MonadResource m
                         , MonadError AWS.Error m
                         , MonadReader Env m
                         )
                      => Text -- ^ 'dei2ElasticIp'
                      -> State DisassociateElasticIp a
                      -> m DisassociateElasticIpResponse
disassociateElasticIp p1 s =
    send $ (mkDisassociateElasticIp p1) &~ s

-- $GetHostnameSuggestion
-- Gets a generated host name for the specified layer, based on the current
-- host name theme. Required Permissions: To use this action, an IAM user must
-- have a Manage permissions level for the stack, or an attached policy that
-- explicitly grants permissions. For more information on user permissions,
-- see Managing User Permissions.
--
-- See: 'Network.AWS.OpsWorks.V2013_02_18.GetHostnameSuggestion'

getHostnameSuggestion :: ( MonadCatch m
                         , MonadResource m
                         , MonadError AWS.Error m
                         , MonadReader Env m
                         )
                      => Text -- ^ 'ghsLayerId'
                      -> State GetHostnameSuggestion a
                      -> m GetHostnameSuggestionResponse
getHostnameSuggestion p1 s =
    send $ (mkGetHostnameSuggestion p1) &~ s

-- $RebootInstance
-- Reboots a specified instance. For more information, see Starting, Stopping,
-- and Rebooting Instances. Required Permissions: To use this action, an IAM
-- user must have a Manage permissions level for the stack, or an attached
-- policy that explicitly grants permissions. For more information on user
-- permissions, see Managing User Permissions.
--
-- See: 'Network.AWS.OpsWorks.V2013_02_18.RebootInstance'

rebootInstance :: ( MonadCatch m
                  , MonadResource m
                  , MonadError AWS.Error m
                  , MonadReader Env m
                  )
               => Text -- ^ 'riInstanceId'
               -> State RebootInstance a
               -> m RebootInstanceResponse
rebootInstance p1 s =
    send $ (mkRebootInstance p1) &~ s

-- $RegisterElasticIp
-- Registers an Elastic IP address with a specified stack. An address can be
-- registered with only one stack at a time. If the address is already
-- registered, you must first deregister it by calling DeregisterElasticIp.
-- For more information, see Resource Management. Required Permissions: To use
-- this action, an IAM user must have a Manage permissions level for the
-- stack, or an attached policy that explicitly grants permissions. For more
-- information on user permissions, see Managing User Permissions.
--
-- See: 'Network.AWS.OpsWorks.V2013_02_18.RegisterElasticIp'

registerElasticIp :: ( MonadCatch m
                     , MonadResource m
                     , MonadError AWS.Error m
                     , MonadReader Env m
                     )
                  => Text -- ^ 'reiElasticIp'
                  -> Text -- ^ 'reiStackId'
                  -> State RegisterElasticIp a
                  -> m RegisterElasticIpResponse
registerElasticIp p1 p2 s =
    send $ (mkRegisterElasticIp p1 p2) &~ s

-- $RegisterRdsDbInstance
-- Registers an Amazon RDS instance with a stack.
--
-- See: 'Network.AWS.OpsWorks.V2013_02_18.RegisterRdsDbInstance'

registerRdsDbInstance :: ( MonadCatch m
                         , MonadResource m
                         , MonadError AWS.Error m
                         , MonadReader Env m
                         )
                      => Text -- ^ 'rrdiStackId'
                      -> Text -- ^ 'rrdiRdsDbInstanceArn'
                      -> Text -- ^ 'rrdiDbUser'
                      -> Text -- ^ 'rrdiDbPassword'
                      -> State RegisterRdsDbInstance a
                      -> m RegisterRdsDbInstanceResponse
registerRdsDbInstance p1 p2 p3 p4 s =
    send $ (mkRegisterRdsDbInstance p1 p2 p3 p4) &~ s

-- $RegisterVolume
-- Registers an Amazon EBS volume with a specified stack. A volume can be
-- registered with only one stack at a time. If the volume is already
-- registered, you must first deregister it by calling DeregisterVolume. For
-- more information, see Resource Management. Required Permissions: To use
-- this action, an IAM user must have a Manage permissions level for the
-- stack, or an attached policy that explicitly grants permissions. For more
-- information on user permissions, see Managing User Permissions.
--
-- See: 'Network.AWS.OpsWorks.V2013_02_18.RegisterVolume'

registerVolume :: ( MonadCatch m
                  , MonadResource m
                  , MonadError AWS.Error m
                  , MonadReader Env m
                  )
               => Text -- ^ 'rvStackId'
               -> State RegisterVolume a
               -> m RegisterVolumeResponse
registerVolume p2 s =
    send $ (mkRegisterVolume p2) &~ s

-- $SetLoadBasedAutoScaling
-- Specify the load-based auto scaling configuration for a specified layer.
-- For more information, see Managing Load with Time-based and Load-based
-- Instances. To use load-based auto scaling, you must create a set of
-- load-based auto scaling instances. Load-based auto scaling operates only on
-- the instances from that set, so you must ensure that you have created
-- enough instances to handle the maximum anticipated load. Required
-- Permissions: To use this action, an IAM user must have a Manage permissions
-- level for the stack, or an attached policy that explicitly grants
-- permissions. For more information on user permissions, see Managing User
-- Permissions.
--
-- See: 'Network.AWS.OpsWorks.V2013_02_18.SetLoadBasedAutoScaling'

setLoadBasedAutoScaling :: ( MonadCatch m
                           , MonadResource m
                           , MonadError AWS.Error m
                           , MonadReader Env m
                           )
                        => Text -- ^ 'slbasLayerId'
                        -> State SetLoadBasedAutoScaling a
                        -> m SetLoadBasedAutoScalingResponse
setLoadBasedAutoScaling p1 s =
    send $ (mkSetLoadBasedAutoScaling p1) &~ s

-- $SetPermission
-- Specifies a user's permissions. For more information, see Security and
-- Permissions. Required Permissions: To use this action, an IAM user must
-- have a Manage permissions level for the stack, or an attached policy that
-- explicitly grants permissions. For more information on user permissions,
-- see Managing User Permissions.
--
-- See: 'Network.AWS.OpsWorks.V2013_02_18.SetPermission'

setPermission :: ( MonadCatch m
                 , MonadResource m
                 , MonadError AWS.Error m
                 , MonadReader Env m
                 )
              => Text -- ^ 'spStackId'
              -> Text -- ^ 'spIamUserArn'
              -> State SetPermission a
              -> m SetPermissionResponse
setPermission p1 p2 s =
    send $ (mkSetPermission p1 p2) &~ s

-- $SetTimeBasedAutoScaling
-- Specify the time-based auto scaling configuration for a specified instance.
-- For more information, see Managing Load with Time-based and Load-based
-- Instances. Required Permissions: To use this action, an IAM user must have
-- a Manage permissions level for the stack, or an attached policy that
-- explicitly grants permissions. For more information on user permissions,
-- see Managing User Permissions.
--
-- See: 'Network.AWS.OpsWorks.V2013_02_18.SetTimeBasedAutoScaling'

setTimeBasedAutoScaling :: ( MonadCatch m
                           , MonadResource m
                           , MonadError AWS.Error m
                           , MonadReader Env m
                           )
                        => Text -- ^ 'stbasInstanceId'
                        -> State SetTimeBasedAutoScaling a
                        -> m SetTimeBasedAutoScalingResponse
setTimeBasedAutoScaling p1 s =
    send $ (mkSetTimeBasedAutoScaling p1) &~ s

-- $StartInstance
-- Starts a specified instance. For more information, see Starting, Stopping,
-- and Rebooting Instances. Required Permissions: To use this action, an IAM
-- user must have a Manage permissions level for the stack, or an attached
-- policy that explicitly grants permissions. For more information on user
-- permissions, see Managing User Permissions.
--
-- See: 'Network.AWS.OpsWorks.V2013_02_18.StartInstance'

startInstance :: ( MonadCatch m
                 , MonadResource m
                 , MonadError AWS.Error m
                 , MonadReader Env m
                 )
              => Text -- ^ 'siInstanceId'
              -> State StartInstance a
              -> m StartInstanceResponse
startInstance p1 s =
    send $ (mkStartInstance p1) &~ s

-- $StartStack
-- Starts a stack's instances. Required Permissions: To use this action, an
-- IAM user must have a Manage permissions level for the stack, or an attached
-- policy that explicitly grants permissions. For more information on user
-- permissions, see Managing User Permissions.
--
-- See: 'Network.AWS.OpsWorks.V2013_02_18.StartStack'

startStack :: ( MonadCatch m
              , MonadResource m
              , MonadError AWS.Error m
              , MonadReader Env m
              )
           => Text -- ^ 'ss1StackId'
           -> State StartStack a
           -> m StartStackResponse
startStack p1 s =
    send $ (mkStartStack p1) &~ s

-- $StopInstance
-- Stops a specified instance. When you stop a standard instance, the data
-- disappears and must be reinstalled when you restart the instance. You can
-- stop an Amazon EBS-backed instance without losing data. For more
-- information, see Starting, Stopping, and Rebooting Instances. Required
-- Permissions: To use this action, an IAM user must have a Manage permissions
-- level for the stack, or an attached policy that explicitly grants
-- permissions. For more information on user permissions, see Managing User
-- Permissions.
--
-- See: 'Network.AWS.OpsWorks.V2013_02_18.StopInstance'

stopInstance :: ( MonadCatch m
                , MonadResource m
                , MonadError AWS.Error m
                , MonadReader Env m
                )
             => Text -- ^ 'si1InstanceId'
             -> State StopInstance a
             -> m StopInstanceResponse
stopInstance p1 s =
    send $ (mkStopInstance p1) &~ s

-- $StopStack
-- Stops a specified stack. Required Permissions: To use this action, an IAM
-- user must have a Manage permissions level for the stack, or an attached
-- policy that explicitly grants permissions. For more information on user
-- permissions, see Managing User Permissions.
--
-- See: 'Network.AWS.OpsWorks.V2013_02_18.StopStack'

stopStack :: ( MonadCatch m
             , MonadResource m
             , MonadError AWS.Error m
             , MonadReader Env m
             )
          => Text -- ^ 'ss2StackId'
          -> State StopStack a
          -> m StopStackResponse
stopStack p1 s =
    send $ (mkStopStack p1) &~ s

-- $UnassignVolume
-- Unassigns an assigned Amazon EBS volume. The volume remains registered with
-- the stack. For more information, see Resource Management. Required
-- Permissions: To use this action, an IAM user must have a Manage permissions
-- level for the stack, or an attached policy that explicitly grants
-- permissions. For more information on user permissions, see Managing User
-- Permissions.
--
-- See: 'Network.AWS.OpsWorks.V2013_02_18.UnassignVolume'

unassignVolume :: ( MonadCatch m
                  , MonadResource m
                  , MonadError AWS.Error m
                  , MonadReader Env m
                  )
               => Text -- ^ 'uvVolumeId'
               -> State UnassignVolume a
               -> m UnassignVolumeResponse
unassignVolume p1 s =
    send $ (mkUnassignVolume p1) &~ s

-- $UpdateApp
-- Updates a specified app. Required Permissions: To use this action, an IAM
-- user must have a Deploy or Manage permissions level for the stack, or an
-- attached policy that explicitly grants permissions. For more information on
-- user permissions, see Managing User Permissions.
--
-- See: 'Network.AWS.OpsWorks.V2013_02_18.UpdateApp'

updateApp :: ( MonadCatch m
             , MonadResource m
             , MonadError AWS.Error m
             , MonadReader Env m
             )
          => Text -- ^ 'uaAppId'
          -> State UpdateApp a
          -> m UpdateAppResponse
updateApp p1 s =
    send $ (mkUpdateApp p1) &~ s

-- $UpdateElasticIp
-- Updates a registered Elastic IP address's name. For more information, see
-- Resource Management. Required Permissions: To use this action, an IAM user
-- must have a Manage permissions level for the stack, or an attached policy
-- that explicitly grants permissions. For more information on user
-- permissions, see Managing User Permissions.
--
-- See: 'Network.AWS.OpsWorks.V2013_02_18.UpdateElasticIp'

updateElasticIp :: ( MonadCatch m
                   , MonadResource m
                   , MonadError AWS.Error m
                   , MonadReader Env m
                   )
                => Text -- ^ 'ueiElasticIp'
                -> State UpdateElasticIp a
                -> m UpdateElasticIpResponse
updateElasticIp p1 s =
    send $ (mkUpdateElasticIp p1) &~ s

-- $UpdateInstance
-- Updates a specified instance. Required Permissions: To use this action, an
-- IAM user must have a Manage permissions level for the stack, or an attached
-- policy that explicitly grants permissions. For more information on user
-- permissions, see Managing User Permissions.
--
-- See: 'Network.AWS.OpsWorks.V2013_02_18.UpdateInstance'

updateInstance :: ( MonadCatch m
                  , MonadResource m
                  , MonadError AWS.Error m
                  , MonadReader Env m
                  )
               => Text -- ^ 'uiInstanceId'
               -> State UpdateInstance a
               -> m UpdateInstanceResponse
updateInstance p1 s =
    send $ (mkUpdateInstance p1) &~ s

-- $UpdateLayer
-- Updates a specified layer. Required Permissions: To use this action, an IAM
-- user must have a Manage permissions level for the stack, or an attached
-- policy that explicitly grants permissions. For more information on user
-- permissions, see Managing User Permissions.
--
-- See: 'Network.AWS.OpsWorks.V2013_02_18.UpdateLayer'

updateLayer :: ( MonadCatch m
               , MonadResource m
               , MonadError AWS.Error m
               , MonadReader Env m
               )
            => Text -- ^ 'ulLayerId'
            -> State UpdateLayer a
            -> m UpdateLayerResponse
updateLayer p1 s =
    send $ (mkUpdateLayer p1) &~ s

-- $UpdateMyUserProfile
-- Updates a user's SSH public key. Required Permissions: To use this action,
-- an IAM user must have self-management enabled or an attached policy that
-- explicitly grants permissions. For more information on user permissions,
-- see Managing User Permissions.
--
-- See: 'Network.AWS.OpsWorks.V2013_02_18.UpdateMyUserProfile'

updateMyUserProfile :: ( MonadCatch m
                       , MonadResource m
                       , MonadError AWS.Error m
                       , MonadReader Env m
                       )
                    => State UpdateMyUserProfile a
                    -> m UpdateMyUserProfileResponse
updateMyUserProfile s =
    send (mkUpdateMyUserProfile &~ s)

-- $UpdateRdsDbInstance
-- Updates an Amazon RDS instance.
--
-- See: 'Network.AWS.OpsWorks.V2013_02_18.UpdateRdsDbInstance'

updateRdsDbInstance :: ( MonadCatch m
                       , MonadResource m
                       , MonadError AWS.Error m
                       , MonadReader Env m
                       )
                    => Text -- ^ 'urdiRdsDbInstanceArn'
                    -> State UpdateRdsDbInstance a
                    -> m UpdateRdsDbInstanceResponse
updateRdsDbInstance p1 s =
    send $ (mkUpdateRdsDbInstance p1) &~ s

-- $UpdateStack
-- Updates a specified stack. Required Permissions: To use this action, an IAM
-- user must have a Manage permissions level for the stack, or an attached
-- policy that explicitly grants permissions. For more information on user
-- permissions, see Managing User Permissions.
--
-- See: 'Network.AWS.OpsWorks.V2013_02_18.UpdateStack'

updateStack :: ( MonadCatch m
               , MonadResource m
               , MonadError AWS.Error m
               , MonadReader Env m
               )
            => Text -- ^ 'usStackId'
            -> State UpdateStack a
            -> m UpdateStackResponse
updateStack p1 s =
    send $ (mkUpdateStack p1) &~ s

-- $UpdateUserProfile
-- Updates a specified user profile. Required Permissions: To use this action,
-- an IAM user must have an attached policy that explicitly grants
-- permissions. For more information on user permissions, see Managing User
-- Permissions.
--
-- See: 'Network.AWS.OpsWorks.V2013_02_18.UpdateUserProfile'

updateUserProfile :: ( MonadCatch m
                     , MonadResource m
                     , MonadError AWS.Error m
                     , MonadReader Env m
                     )
                  => Text -- ^ 'uupIamUserArn'
                  -> State UpdateUserProfile a
                  -> m UpdateUserProfileResponse
updateUserProfile p1 s =
    send $ (mkUpdateUserProfile p1) &~ s

-- $UpdateVolume
-- Updates an Amazon EBS volume's name or mount point. For more information,
-- see Resource Management. Required Permissions: To use this action, an IAM
-- user must have a Manage permissions level for the stack, or an attached
-- policy that explicitly grants permissions. For more information on user
-- permissions, see Managing User Permissions.
--
-- See: 'Network.AWS.OpsWorks.V2013_02_18.UpdateVolume'

updateVolume :: ( MonadCatch m
                , MonadResource m
                , MonadError AWS.Error m
                , MonadReader Env m
                )
             => Text -- ^ 'uv1VolumeId'
             -> State UpdateVolume a
             -> m UpdateVolumeResponse
updateVolume p1 s =
    send $ (mkUpdateVolume p1) &~ s
