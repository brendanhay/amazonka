-- Module      : Network.AWS.ElasticBeanstalk
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
module Network.AWS.ElasticBeanstalk
    ( module Network.AWS.ElasticBeanstalk.CheckDNSAvailability
    , module Network.AWS.ElasticBeanstalk.CreateApplication
    , module Network.AWS.ElasticBeanstalk.CreateApplicationVersion
    , module Network.AWS.ElasticBeanstalk.CreateConfigurationTemplate
    , module Network.AWS.ElasticBeanstalk.CreateEnvironment
    , module Network.AWS.ElasticBeanstalk.CreateStorageLocation
    , module Network.AWS.ElasticBeanstalk.DeleteApplication
    , module Network.AWS.ElasticBeanstalk.DeleteApplicationVersion
    , module Network.AWS.ElasticBeanstalk.DeleteConfigurationTemplate
    , module Network.AWS.ElasticBeanstalk.DeleteEnvironmentConfiguration
    , module Network.AWS.ElasticBeanstalk.DescribeApplicationVersions
    , module Network.AWS.ElasticBeanstalk.DescribeApplications
    , module Network.AWS.ElasticBeanstalk.DescribeConfigurationOptions
    , module Network.AWS.ElasticBeanstalk.DescribeConfigurationSettings
    , module Network.AWS.ElasticBeanstalk.DescribeEnvironmentResources
    , module Network.AWS.ElasticBeanstalk.DescribeEnvironments
    , module Network.AWS.ElasticBeanstalk.DescribeEvents
    , module Network.AWS.ElasticBeanstalk.ListAvailableSolutionStacks
    , module Network.AWS.ElasticBeanstalk.Monadic
    , module Network.AWS.ElasticBeanstalk.RebuildEnvironment
    , module Network.AWS.ElasticBeanstalk.RequestEnvironmentInfo
    , module Network.AWS.ElasticBeanstalk.RestartAppServer
    , module Network.AWS.ElasticBeanstalk.RetrieveEnvironmentInfo
    , module Network.AWS.ElasticBeanstalk.SwapEnvironmentCNAMEs
    , module Network.AWS.ElasticBeanstalk.TerminateEnvironment
    , module Network.AWS.ElasticBeanstalk.Types
    , module Network.AWS.ElasticBeanstalk.UpdateApplication
    , module Network.AWS.ElasticBeanstalk.UpdateApplicationVersion
    , module Network.AWS.ElasticBeanstalk.UpdateConfigurationTemplate
    , module Network.AWS.ElasticBeanstalk.UpdateEnvironment
    , module Network.AWS.ElasticBeanstalk.ValidateConfigurationSettings
    ) where

import Network.AWS.ElasticBeanstalk.CheckDNSAvailability
import Network.AWS.ElasticBeanstalk.CreateApplication
import Network.AWS.ElasticBeanstalk.CreateApplicationVersion
import Network.AWS.ElasticBeanstalk.CreateConfigurationTemplate
import Network.AWS.ElasticBeanstalk.CreateEnvironment
import Network.AWS.ElasticBeanstalk.CreateStorageLocation
import Network.AWS.ElasticBeanstalk.DeleteApplication
import Network.AWS.ElasticBeanstalk.DeleteApplicationVersion
import Network.AWS.ElasticBeanstalk.DeleteConfigurationTemplate
import Network.AWS.ElasticBeanstalk.DeleteEnvironmentConfiguration
import Network.AWS.ElasticBeanstalk.DescribeApplicationVersions
import Network.AWS.ElasticBeanstalk.DescribeApplications
import Network.AWS.ElasticBeanstalk.DescribeConfigurationOptions
import Network.AWS.ElasticBeanstalk.DescribeConfigurationSettings
import Network.AWS.ElasticBeanstalk.DescribeEnvironmentResources
import Network.AWS.ElasticBeanstalk.DescribeEnvironments
import Network.AWS.ElasticBeanstalk.DescribeEvents
import Network.AWS.ElasticBeanstalk.ListAvailableSolutionStacks
import Network.AWS.ElasticBeanstalk.Monadic
import Network.AWS.ElasticBeanstalk.RebuildEnvironment
import Network.AWS.ElasticBeanstalk.RequestEnvironmentInfo
import Network.AWS.ElasticBeanstalk.RestartAppServer
import Network.AWS.ElasticBeanstalk.RetrieveEnvironmentInfo
import Network.AWS.ElasticBeanstalk.SwapEnvironmentCNAMEs
import Network.AWS.ElasticBeanstalk.TerminateEnvironment
import Network.AWS.ElasticBeanstalk.Types
import Network.AWS.ElasticBeanstalk.UpdateApplication
import Network.AWS.ElasticBeanstalk.UpdateApplicationVersion
import Network.AWS.ElasticBeanstalk.UpdateConfigurationTemplate
import Network.AWS.ElasticBeanstalk.UpdateEnvironment
import Network.AWS.ElasticBeanstalk.ValidateConfigurationSettings
