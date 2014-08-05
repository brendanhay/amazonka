-- Module      : Network.AWS.ElasticBeanstalk.V2010_12_01
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | AWS Elastic Beanstalk is an easy-to-use service for deploying and scaling
-- web applications and services developed with Java, .NET, PHP, Node.js,
-- Python, Ruby, and Docker on familiar servers such as Apache HTTP Server,
-- Apache Tomcat, Nginx, Passenger, and IIS 7.5/8. You can simply upload your
-- code and Elastic Beanstalk automatically handles the deployment, from
-- capacity provisioning, load balancing, auto-scaling to application health
-- monitoring. At the same time, you retain full control over the AWS
-- resources powering your application and can access the underlying resources
-- at any time. There is no additional charge for Elastic Beanstalk - you pay
-- only for the AWS resources needed to store and run your applications.
module Network.AWS.ElasticBeanstalk.V2010_12_01 (module Export) where

import Network.AWS.ElasticBeanstalk.V2010_12_01.CheckDNSAvailability as Export
import Network.AWS.ElasticBeanstalk.V2010_12_01.CreateApplication as Export
import Network.AWS.ElasticBeanstalk.V2010_12_01.CreateApplicationVersion as Export
import Network.AWS.ElasticBeanstalk.V2010_12_01.CreateConfigurationTemplate as Export
import Network.AWS.ElasticBeanstalk.V2010_12_01.CreateEnvironment as Export
import Network.AWS.ElasticBeanstalk.V2010_12_01.CreateStorageLocation as Export
import Network.AWS.ElasticBeanstalk.V2010_12_01.DeleteApplication as Export
import Network.AWS.ElasticBeanstalk.V2010_12_01.DeleteApplicationVersion as Export
import Network.AWS.ElasticBeanstalk.V2010_12_01.DeleteConfigurationTemplate as Export
import Network.AWS.ElasticBeanstalk.V2010_12_01.DeleteEnvironmentConfiguration as Export
import Network.AWS.ElasticBeanstalk.V2010_12_01.DescribeApplicationVersions as Export
import Network.AWS.ElasticBeanstalk.V2010_12_01.DescribeApplications as Export
import Network.AWS.ElasticBeanstalk.V2010_12_01.DescribeConfigurationOptions as Export
import Network.AWS.ElasticBeanstalk.V2010_12_01.DescribeConfigurationSettings as Export
import Network.AWS.ElasticBeanstalk.V2010_12_01.DescribeEnvironmentResources as Export
import Network.AWS.ElasticBeanstalk.V2010_12_01.DescribeEnvironments as Export
import Network.AWS.ElasticBeanstalk.V2010_12_01.DescribeEvents as Export
import Network.AWS.ElasticBeanstalk.V2010_12_01.ListAvailableSolutionStacks as Export
import Network.AWS.ElasticBeanstalk.V2010_12_01.RebuildEnvironment as Export
import Network.AWS.ElasticBeanstalk.V2010_12_01.RequestEnvironmentInfo as Export
import Network.AWS.ElasticBeanstalk.V2010_12_01.RestartAppServer as Export
import Network.AWS.ElasticBeanstalk.V2010_12_01.RetrieveEnvironmentInfo as Export
import Network.AWS.ElasticBeanstalk.V2010_12_01.SwapEnvironmentCNAMEs as Export
import Network.AWS.ElasticBeanstalk.V2010_12_01.TerminateEnvironment as Export
import Network.AWS.ElasticBeanstalk.V2010_12_01.Types as Export
import Network.AWS.ElasticBeanstalk.V2010_12_01.UpdateApplication as Export
import Network.AWS.ElasticBeanstalk.V2010_12_01.UpdateApplicationVersion as Export
import Network.AWS.ElasticBeanstalk.V2010_12_01.UpdateConfigurationTemplate as Export
import Network.AWS.ElasticBeanstalk.V2010_12_01.UpdateEnvironment as Export
import Network.AWS.ElasticBeanstalk.V2010_12_01.ValidateConfigurationSettings as Export
