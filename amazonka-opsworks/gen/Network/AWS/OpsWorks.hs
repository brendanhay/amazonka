-- Module      : Network.AWS.OpsWorks
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | AWS OpsWorks
--
-- Welcome to the /AWS OpsWorks API Reference/. This guide provides
-- descriptions, syntax, and usage examples about AWS OpsWorks actions and
-- data types, including common parameters and error codes.
--
-- AWS OpsWorks is an application management service that provides an
-- integrated experience for overseeing the complete application lifecycle.
-- For information about this product, go to the
-- <http://aws.amazon.com/opsworks/ AWS OpsWorks> details page.
--
-- __SDKs and CLI__
--
-- The most common way to use the AWS OpsWorks API is by using the AWS
-- Command Line Interface (CLI) or by using one of the AWS SDKs to
-- implement applications in your preferred language. For more information,
-- see:
--
-- -   <http://docs.aws.amazon.com/cli/latest/userguide/cli-chap-welcome.html AWS CLI>
-- -   <http://docs.aws.amazon.com/AWSJavaSDK/latest/javadoc/com/amazonaws/services/opsworks/AWSOpsWorksClient.html AWS SDK for Java>
-- -   <http://docs.aws.amazon.com/sdkfornet/latest/apidocs/html/N_Amazon_OpsWorks.htm AWS SDK for .NET>
-- -   <http://docs.aws.amazon.com/aws-sdk-php-2/latest/class-Aws.OpsWorks.OpsWorksClient.html AWS SDK for PHP 2>
-- -   <http://docs.aws.amazon.com/AWSRubySDK/latest/AWS/OpsWorks/Client.html AWS SDK for Ruby>
-- -   <http://aws.amazon.com/documentation/sdkforjavascript/ AWS SDK for Node.js>
-- -   <http://docs.pythonboto.org/en/latest/ref/opsworks.html AWS SDK for Python(Boto)>
--
-- __Endpoints__
--
-- AWS OpsWorks supports only one endpoint,
-- opsworks.us-east-1.amazonaws.com (HTTPS), so you must connect to that
-- endpoint. You can then use the API to direct AWS OpsWorks to create
-- stacks in any AWS Region.
--
-- __Chef Versions__
--
-- When you call CreateStack, CloneStack, or UpdateStack we recommend you
-- use the @ConfigurationManager@ parameter to specify the Chef version,
-- 0.9, 11.4, or 11.10. The default value is currently 11.10. For more
-- information, see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/workingcookbook-chef11.html Chef Versions>.
--
-- You can still specify Chef 0.9 for your stack, but new features are not
-- available for Chef 0.9 stacks, and support is scheduled to end on July
-- 24, 2014. We do not recommend using Chef 0.9 for new stacks, and we
-- recommend migrating your existing Chef 0.9 stacks to Chef 11.10 as soon
-- as possible.
module Network.AWS.OpsWorks
    ( module Export
    ) where

import           Network.AWS.OpsWorks.AssignInstance                      as Export
import           Network.AWS.OpsWorks.AssignVolume                        as Export
import           Network.AWS.OpsWorks.AssociateElasticIP                  as Export
import           Network.AWS.OpsWorks.AttachElasticLoadBalancer           as Export
import           Network.AWS.OpsWorks.CloneStack                          as Export
import           Network.AWS.OpsWorks.CreateApp                           as Export
import           Network.AWS.OpsWorks.CreateDeployment                    as Export
import           Network.AWS.OpsWorks.CreateInstance                      as Export
import           Network.AWS.OpsWorks.CreateLayer                         as Export
import           Network.AWS.OpsWorks.CreateStack                         as Export
import           Network.AWS.OpsWorks.CreateUserProfile                   as Export
import           Network.AWS.OpsWorks.DeleteApp                           as Export
import           Network.AWS.OpsWorks.DeleteInstance                      as Export
import           Network.AWS.OpsWorks.DeleteLayer                         as Export
import           Network.AWS.OpsWorks.DeleteStack                         as Export
import           Network.AWS.OpsWorks.DeleteUserProfile                   as Export
import           Network.AWS.OpsWorks.DeregisterElasticIP                 as Export
import           Network.AWS.OpsWorks.DeregisterInstance                  as Export
import           Network.AWS.OpsWorks.DeregisterRDSDBInstance             as Export
import           Network.AWS.OpsWorks.DeregisterVolume                    as Export
import           Network.AWS.OpsWorks.DescribeAgentVersions               as Export
import           Network.AWS.OpsWorks.DescribeApps                        as Export
import           Network.AWS.OpsWorks.DescribeCommands                    as Export
import           Network.AWS.OpsWorks.DescribeDeployments                 as Export
import           Network.AWS.OpsWorks.DescribeElasticIPs                  as Export
import           Network.AWS.OpsWorks.DescribeElasticLoadBalancers        as Export
import           Network.AWS.OpsWorks.DescribeInstances                   as Export
import           Network.AWS.OpsWorks.DescribeLayers                      as Export
import           Network.AWS.OpsWorks.DescribeLoadBasedAutoScaling        as Export
import           Network.AWS.OpsWorks.DescribeMyUserProfile               as Export
import           Network.AWS.OpsWorks.DescribePermissions                 as Export
import           Network.AWS.OpsWorks.DescribeRAIDArrays                  as Export
import           Network.AWS.OpsWorks.DescribeRDSDBInstances              as Export
import           Network.AWS.OpsWorks.DescribeServiceErrors               as Export
import           Network.AWS.OpsWorks.DescribeStackProvisioningParameters as Export
import           Network.AWS.OpsWorks.DescribeStacks                      as Export
import           Network.AWS.OpsWorks.DescribeStackSummary                as Export
import           Network.AWS.OpsWorks.DescribeTimeBasedAutoScaling        as Export
import           Network.AWS.OpsWorks.DescribeUserProfiles                as Export
import           Network.AWS.OpsWorks.DescribeVolumes                     as Export
import           Network.AWS.OpsWorks.DetachElasticLoadBalancer           as Export
import           Network.AWS.OpsWorks.DisassociateElasticIP               as Export
import           Network.AWS.OpsWorks.GetHostnameSuggestion               as Export
import           Network.AWS.OpsWorks.GrantAccess                         as Export
import           Network.AWS.OpsWorks.RebootInstance                      as Export
import           Network.AWS.OpsWorks.RegisterElasticIP                   as Export
import           Network.AWS.OpsWorks.RegisterInstance                    as Export
import           Network.AWS.OpsWorks.RegisterRDSDBInstance               as Export
import           Network.AWS.OpsWorks.RegisterVolume                      as Export
import           Network.AWS.OpsWorks.SetLoadBasedAutoScaling             as Export
import           Network.AWS.OpsWorks.SetPermission                       as Export
import           Network.AWS.OpsWorks.SetTimeBasedAutoScaling             as Export
import           Network.AWS.OpsWorks.StartInstance                       as Export
import           Network.AWS.OpsWorks.StartStack                          as Export
import           Network.AWS.OpsWorks.StopInstance                        as Export
import           Network.AWS.OpsWorks.StopStack                           as Export
import           Network.AWS.OpsWorks.Types                               as Export
import           Network.AWS.OpsWorks.UnassignInstance                    as Export
import           Network.AWS.OpsWorks.UnassignVolume                      as Export
import           Network.AWS.OpsWorks.UpdateApp                           as Export
import           Network.AWS.OpsWorks.UpdateElasticIP                     as Export
import           Network.AWS.OpsWorks.UpdateInstance                      as Export
import           Network.AWS.OpsWorks.UpdateLayer                         as Export
import           Network.AWS.OpsWorks.UpdateMyUserProfile                 as Export
import           Network.AWS.OpsWorks.UpdateRDSDBInstance                 as Export
import           Network.AWS.OpsWorks.UpdateStack                         as Export
import           Network.AWS.OpsWorks.UpdateUserProfile                   as Export
import           Network.AWS.OpsWorks.UpdateVolume                        as Export
import           Network.AWS.OpsWorks.Waiters                             as Export
