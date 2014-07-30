{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
module Network.AWS.ElasticBeanstalk.V2010_12_01
    ( module Network.AWS.ElasticBeanstalk.V2010_12_01.CheckDNSAvailability
    , module Network.AWS.ElasticBeanstalk.V2010_12_01.CreateApplication
    , module Network.AWS.ElasticBeanstalk.V2010_12_01.CreateApplicationVersion
    , module Network.AWS.ElasticBeanstalk.V2010_12_01.CreateConfigurationTemplate
    , module Network.AWS.ElasticBeanstalk.V2010_12_01.CreateEnvironment
    , module Network.AWS.ElasticBeanstalk.V2010_12_01.CreateStorageLocation
    , module Network.AWS.ElasticBeanstalk.V2010_12_01.DeleteApplication
    , module Network.AWS.ElasticBeanstalk.V2010_12_01.DeleteApplicationVersion
    , module Network.AWS.ElasticBeanstalk.V2010_12_01.DeleteConfigurationTemplate
    , module Network.AWS.ElasticBeanstalk.V2010_12_01.DeleteEnvironmentConfiguration
    , module Network.AWS.ElasticBeanstalk.V2010_12_01.DescribeApplicationVersions
    , module Network.AWS.ElasticBeanstalk.V2010_12_01.DescribeApplications
    , module Network.AWS.ElasticBeanstalk.V2010_12_01.DescribeConfigurationOptions
    , module Network.AWS.ElasticBeanstalk.V2010_12_01.DescribeConfigurationSettings
    , module Network.AWS.ElasticBeanstalk.V2010_12_01.DescribeEnvironmentResources
    , module Network.AWS.ElasticBeanstalk.V2010_12_01.DescribeEnvironments
    , module Network.AWS.ElasticBeanstalk.V2010_12_01.DescribeEvents
    , module Network.AWS.ElasticBeanstalk.V2010_12_01.Lenses
    , module Network.AWS.ElasticBeanstalk.V2010_12_01.ListAvailableSolutionStacks
    , module Network.AWS.ElasticBeanstalk.V2010_12_01.RebuildEnvironment
    , module Network.AWS.ElasticBeanstalk.V2010_12_01.RequestEnvironmentInfo
    , module Network.AWS.ElasticBeanstalk.V2010_12_01.RestartAppServer
    , module Network.AWS.ElasticBeanstalk.V2010_12_01.RetrieveEnvironmentInfo
    , module Network.AWS.ElasticBeanstalk.V2010_12_01.SwapEnvironmentCNAMEs
    , module Network.AWS.ElasticBeanstalk.V2010_12_01.TerminateEnvironment
    , module Network.AWS.ElasticBeanstalk.V2010_12_01.Types
    , module Network.AWS.ElasticBeanstalk.V2010_12_01.UpdateApplication
    , module Network.AWS.ElasticBeanstalk.V2010_12_01.UpdateApplicationVersion
    , module Network.AWS.ElasticBeanstalk.V2010_12_01.UpdateConfigurationTemplate
    , module Network.AWS.ElasticBeanstalk.V2010_12_01.UpdateEnvironment
    , module Network.AWS.ElasticBeanstalk.V2010_12_01.ValidateConfigurationSettings
    ) where

import Network.AWS.ElasticBeanstalk.V2010_12_01.CheckDNSAvailability
import Network.AWS.ElasticBeanstalk.V2010_12_01.CreateApplication
import Network.AWS.ElasticBeanstalk.V2010_12_01.CreateApplicationVersion
import Network.AWS.ElasticBeanstalk.V2010_12_01.CreateConfigurationTemplate
import Network.AWS.ElasticBeanstalk.V2010_12_01.CreateEnvironment
import Network.AWS.ElasticBeanstalk.V2010_12_01.CreateStorageLocation
import Network.AWS.ElasticBeanstalk.V2010_12_01.DeleteApplication
import Network.AWS.ElasticBeanstalk.V2010_12_01.DeleteApplicationVersion
import Network.AWS.ElasticBeanstalk.V2010_12_01.DeleteConfigurationTemplate
import Network.AWS.ElasticBeanstalk.V2010_12_01.DeleteEnvironmentConfiguration
import Network.AWS.ElasticBeanstalk.V2010_12_01.DescribeApplicationVersions
import Network.AWS.ElasticBeanstalk.V2010_12_01.DescribeApplications
import Network.AWS.ElasticBeanstalk.V2010_12_01.DescribeConfigurationOptions
import Network.AWS.ElasticBeanstalk.V2010_12_01.DescribeConfigurationSettings
import Network.AWS.ElasticBeanstalk.V2010_12_01.DescribeEnvironmentResources
import Network.AWS.ElasticBeanstalk.V2010_12_01.DescribeEnvironments
import Network.AWS.ElasticBeanstalk.V2010_12_01.DescribeEvents
import Network.AWS.ElasticBeanstalk.V2010_12_01.Lenses
import Network.AWS.ElasticBeanstalk.V2010_12_01.ListAvailableSolutionStacks
import Network.AWS.ElasticBeanstalk.V2010_12_01.RebuildEnvironment
import Network.AWS.ElasticBeanstalk.V2010_12_01.RequestEnvironmentInfo
import Network.AWS.ElasticBeanstalk.V2010_12_01.RestartAppServer
import Network.AWS.ElasticBeanstalk.V2010_12_01.RetrieveEnvironmentInfo
import Network.AWS.ElasticBeanstalk.V2010_12_01.SwapEnvironmentCNAMEs
import Network.AWS.ElasticBeanstalk.V2010_12_01.TerminateEnvironment
import Network.AWS.ElasticBeanstalk.V2010_12_01.Types
import Network.AWS.ElasticBeanstalk.V2010_12_01.UpdateApplication
import Network.AWS.ElasticBeanstalk.V2010_12_01.UpdateApplicationVersion
import Network.AWS.ElasticBeanstalk.V2010_12_01.UpdateConfigurationTemplate
import Network.AWS.ElasticBeanstalk.V2010_12_01.UpdateEnvironment
import Network.AWS.ElasticBeanstalk.V2010_12_01.ValidateConfigurationSettings
