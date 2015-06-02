-- Module      : Network.AWS.OpsWorks
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | AWS OpsWorks provides a simple and flexible way to create and manage stacks
-- and applications. With AWS OpsWorks, you can provision AWS resources, manage
-- their configuration, deploy applications to those resources, and monitor
-- their health.
module Network.AWS.OpsWorks
    ( module Network.AWS.OpsWorks.AssignInstance
    , module Network.AWS.OpsWorks.AssignVolume
    , module Network.AWS.OpsWorks.AssociateElasticIp
    , module Network.AWS.OpsWorks.AttachElasticLoadBalancer
    , module Network.AWS.OpsWorks.CloneStack
    , module Network.AWS.OpsWorks.CreateApp
    , module Network.AWS.OpsWorks.CreateDeployment
    , module Network.AWS.OpsWorks.CreateInstance
    , module Network.AWS.OpsWorks.CreateLayer
    , module Network.AWS.OpsWorks.CreateStack
    , module Network.AWS.OpsWorks.CreateUserProfile
    , module Network.AWS.OpsWorks.DeleteApp
    , module Network.AWS.OpsWorks.DeleteInstance
    , module Network.AWS.OpsWorks.DeleteLayer
    , module Network.AWS.OpsWorks.DeleteStack
    , module Network.AWS.OpsWorks.DeleteUserProfile
    , module Network.AWS.OpsWorks.DeregisterElasticIp
    , module Network.AWS.OpsWorks.DeregisterInstance
    , module Network.AWS.OpsWorks.DeregisterRdsDbInstance
    , module Network.AWS.OpsWorks.DeregisterVolume
    , module Network.AWS.OpsWorks.DescribeApps
    , module Network.AWS.OpsWorks.DescribeCommands
    , module Network.AWS.OpsWorks.DescribeDeployments
    , module Network.AWS.OpsWorks.DescribeElasticIps
    , module Network.AWS.OpsWorks.DescribeElasticLoadBalancers
    , module Network.AWS.OpsWorks.DescribeInstances
    , module Network.AWS.OpsWorks.DescribeLayers
    , module Network.AWS.OpsWorks.DescribeLoadBasedAutoScaling
    , module Network.AWS.OpsWorks.DescribeMyUserProfile
    , module Network.AWS.OpsWorks.DescribePermissions
    , module Network.AWS.OpsWorks.DescribeRaidArrays
    , module Network.AWS.OpsWorks.DescribeRdsDbInstances
    , module Network.AWS.OpsWorks.DescribeServiceErrors
    , module Network.AWS.OpsWorks.DescribeStackProvisioningParameters
    , module Network.AWS.OpsWorks.DescribeStackSummary
    , module Network.AWS.OpsWorks.DescribeStacks
    , module Network.AWS.OpsWorks.DescribeTimeBasedAutoScaling
    , module Network.AWS.OpsWorks.DescribeUserProfiles
    , module Network.AWS.OpsWorks.DescribeVolumes
    , module Network.AWS.OpsWorks.DetachElasticLoadBalancer
    , module Network.AWS.OpsWorks.DisassociateElasticIp
    , module Network.AWS.OpsWorks.GetHostnameSuggestion
    , module Network.AWS.OpsWorks.GrantAccess
    , module Network.AWS.OpsWorks.RebootInstance
    , module Network.AWS.OpsWorks.RegisterElasticIp
    , module Network.AWS.OpsWorks.RegisterInstance
    , module Network.AWS.OpsWorks.RegisterRdsDbInstance
    , module Network.AWS.OpsWorks.RegisterVolume
    , module Network.AWS.OpsWorks.SetLoadBasedAutoScaling
    , module Network.AWS.OpsWorks.SetPermission
    , module Network.AWS.OpsWorks.SetTimeBasedAutoScaling
    , module Network.AWS.OpsWorks.StartInstance
    , module Network.AWS.OpsWorks.StartStack
    , module Network.AWS.OpsWorks.StopInstance
    , module Network.AWS.OpsWorks.StopStack
    , module Network.AWS.OpsWorks.Types
    , module Network.AWS.OpsWorks.UnassignInstance
    , module Network.AWS.OpsWorks.UnassignVolume
    , module Network.AWS.OpsWorks.UpdateApp
    , module Network.AWS.OpsWorks.UpdateElasticIp
    , module Network.AWS.OpsWorks.UpdateInstance
    , module Network.AWS.OpsWorks.UpdateLayer
    , module Network.AWS.OpsWorks.UpdateMyUserProfile
    , module Network.AWS.OpsWorks.UpdateRdsDbInstance
    , module Network.AWS.OpsWorks.UpdateStack
    , module Network.AWS.OpsWorks.UpdateUserProfile
    , module Network.AWS.OpsWorks.UpdateVolume
    ) where

import Network.AWS.OpsWorks.AssignInstance
import Network.AWS.OpsWorks.AssignVolume
import Network.AWS.OpsWorks.AssociateElasticIp
import Network.AWS.OpsWorks.AttachElasticLoadBalancer
import Network.AWS.OpsWorks.CloneStack
import Network.AWS.OpsWorks.CreateApp
import Network.AWS.OpsWorks.CreateDeployment
import Network.AWS.OpsWorks.CreateInstance
import Network.AWS.OpsWorks.CreateLayer
import Network.AWS.OpsWorks.CreateStack
import Network.AWS.OpsWorks.CreateUserProfile
import Network.AWS.OpsWorks.DeleteApp
import Network.AWS.OpsWorks.DeleteInstance
import Network.AWS.OpsWorks.DeleteLayer
import Network.AWS.OpsWorks.DeleteStack
import Network.AWS.OpsWorks.DeleteUserProfile
import Network.AWS.OpsWorks.DeregisterElasticIp
import Network.AWS.OpsWorks.DeregisterInstance
import Network.AWS.OpsWorks.DeregisterRdsDbInstance
import Network.AWS.OpsWorks.DeregisterVolume
import Network.AWS.OpsWorks.DescribeApps
import Network.AWS.OpsWorks.DescribeCommands
import Network.AWS.OpsWorks.DescribeDeployments
import Network.AWS.OpsWorks.DescribeElasticIps
import Network.AWS.OpsWorks.DescribeElasticLoadBalancers
import Network.AWS.OpsWorks.DescribeInstances
import Network.AWS.OpsWorks.DescribeLayers
import Network.AWS.OpsWorks.DescribeLoadBasedAutoScaling
import Network.AWS.OpsWorks.DescribeMyUserProfile
import Network.AWS.OpsWorks.DescribePermissions
import Network.AWS.OpsWorks.DescribeRaidArrays
import Network.AWS.OpsWorks.DescribeRdsDbInstances
import Network.AWS.OpsWorks.DescribeServiceErrors
import Network.AWS.OpsWorks.DescribeStackProvisioningParameters
import Network.AWS.OpsWorks.DescribeStackSummary
import Network.AWS.OpsWorks.DescribeStacks
import Network.AWS.OpsWorks.DescribeTimeBasedAutoScaling
import Network.AWS.OpsWorks.DescribeUserProfiles
import Network.AWS.OpsWorks.DescribeVolumes
import Network.AWS.OpsWorks.DetachElasticLoadBalancer
import Network.AWS.OpsWorks.DisassociateElasticIp
import Network.AWS.OpsWorks.GetHostnameSuggestion
import Network.AWS.OpsWorks.GrantAccess
import Network.AWS.OpsWorks.RebootInstance
import Network.AWS.OpsWorks.RegisterElasticIp
import Network.AWS.OpsWorks.RegisterInstance
import Network.AWS.OpsWorks.RegisterRdsDbInstance
import Network.AWS.OpsWorks.RegisterVolume
import Network.AWS.OpsWorks.SetLoadBasedAutoScaling
import Network.AWS.OpsWorks.SetPermission
import Network.AWS.OpsWorks.SetTimeBasedAutoScaling
import Network.AWS.OpsWorks.StartInstance
import Network.AWS.OpsWorks.StartStack
import Network.AWS.OpsWorks.StopInstance
import Network.AWS.OpsWorks.StopStack
import Network.AWS.OpsWorks.Types
import Network.AWS.OpsWorks.UnassignInstance
import Network.AWS.OpsWorks.UnassignVolume
import Network.AWS.OpsWorks.UpdateApp
import Network.AWS.OpsWorks.UpdateElasticIp
import Network.AWS.OpsWorks.UpdateInstance
import Network.AWS.OpsWorks.UpdateLayer
import Network.AWS.OpsWorks.UpdateMyUserProfile
import Network.AWS.OpsWorks.UpdateRdsDbInstance
import Network.AWS.OpsWorks.UpdateStack
import Network.AWS.OpsWorks.UpdateUserProfile
import Network.AWS.OpsWorks.UpdateVolume
