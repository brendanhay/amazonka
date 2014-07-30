{-# LANGUAGE TemplateHaskell             #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElasticBeanstalk.V2010_12_01.Lenses
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.ElasticBeanstalk.V2010_12_01.Lenses where

import Control.Lens.TH
import Network.AWS.ElasticBeanstalk.V2010_12_01.Types
import Network.AWS.ElasticBeanstalk.V2010_12_01.DescribeApplications
import Network.AWS.ElasticBeanstalk.V2010_12_01.UpdateEnvironment
import Network.AWS.ElasticBeanstalk.V2010_12_01.TerminateEnvironment
import Network.AWS.ElasticBeanstalk.V2010_12_01.CreateApplicationVersion
import Network.AWS.ElasticBeanstalk.V2010_12_01.DescribeEvents
import Network.AWS.ElasticBeanstalk.V2010_12_01.RequestEnvironmentInfo
import Network.AWS.ElasticBeanstalk.V2010_12_01.RetrieveEnvironmentInfo
import Network.AWS.ElasticBeanstalk.V2010_12_01.DeleteApplication
import Network.AWS.ElasticBeanstalk.V2010_12_01.UpdateApplication
import Network.AWS.ElasticBeanstalk.V2010_12_01.CreateApplication
import Network.AWS.ElasticBeanstalk.V2010_12_01.DeleteConfigurationTemplate
import Network.AWS.ElasticBeanstalk.V2010_12_01.UpdateConfigurationTemplate
import Network.AWS.ElasticBeanstalk.V2010_12_01.DescribeEnvironmentResources
import Network.AWS.ElasticBeanstalk.V2010_12_01.DeleteApplicationVersion
import Network.AWS.ElasticBeanstalk.V2010_12_01.UpdateApplicationVersion
import Network.AWS.ElasticBeanstalk.V2010_12_01.CreateConfigurationTemplate
import Network.AWS.ElasticBeanstalk.V2010_12_01.RebuildEnvironment
import Network.AWS.ElasticBeanstalk.V2010_12_01.DeleteEnvironmentConfiguration
import Network.AWS.ElasticBeanstalk.V2010_12_01.SwapEnvironmentCNAMEs
import Network.AWS.ElasticBeanstalk.V2010_12_01.ListAvailableSolutionStacks
import Network.AWS.ElasticBeanstalk.V2010_12_01.DescribeConfigurationOptions
import Network.AWS.ElasticBeanstalk.V2010_12_01.CreateStorageLocation
import Network.AWS.ElasticBeanstalk.V2010_12_01.DescribeConfigurationSettings
import Network.AWS.ElasticBeanstalk.V2010_12_01.ValidateConfigurationSettings
import Network.AWS.ElasticBeanstalk.V2010_12_01.RestartAppServer
import Network.AWS.ElasticBeanstalk.V2010_12_01.DescribeEnvironments
import Network.AWS.ElasticBeanstalk.V2010_12_01.CheckDNSAvailability
import Network.AWS.ElasticBeanstalk.V2010_12_01.DescribeApplicationVersions
import Network.AWS.ElasticBeanstalk.V2010_12_01.CreateEnvironment

-- Newtypes
makeIso ''AutoScalingGroup
makeIso ''EnvironmentResourcesDescription
makeIso ''Instance
makeIso ''LaunchConfiguration
makeIso ''LoadBalancer
makeIso ''Trigger

-- Products
makeLenses ''ApplicationDescription
makeLenses ''ApplicationVersionDescription
makeLenses ''ConfigurationOptionDescription
makeLenses ''ConfigurationOptionSetting
makeLenses ''ConfigurationSettingsDescription
makeLenses ''EnvironmentDescription
makeLenses ''EnvironmentInfoDescription
makeLenses ''EnvironmentResourceDescription
makeLenses ''EnvironmentTier
makeLenses ''EventDescription
makeLenses ''Listener
makeLenses ''LoadBalancerDescription
makeLenses ''OptionRestrictionRegex
makeLenses ''OptionSpecification
makeLenses ''Queue
makeLenses ''S3Location
makeLenses ''SolutionStackDescription
makeLenses ''SourceConfiguration
makeLenses ''Tag
makeLenses ''ValidationMessage

-- Requests
makeLenses ''DescribeApplications
makeLenses ''UpdateEnvironment
makeLenses ''TerminateEnvironment
makeLenses ''CreateApplicationVersion
makeLenses ''DescribeEvents
makeLenses ''RequestEnvironmentInfo
makeLenses ''RetrieveEnvironmentInfo
makeLenses ''DeleteApplication
makeLenses ''UpdateApplication
makeLenses ''CreateApplication
makeLenses ''DeleteConfigurationTemplate
makeLenses ''UpdateConfigurationTemplate
makeLenses ''DescribeEnvironmentResources
makeLenses ''DeleteApplicationVersion
makeLenses ''UpdateApplicationVersion
makeLenses ''CreateConfigurationTemplate
makeLenses ''RebuildEnvironment
makeLenses ''DeleteEnvironmentConfiguration
makeLenses ''SwapEnvironmentCNAMEs
makeLenses ''ListAvailableSolutionStacks
makeLenses ''DescribeConfigurationOptions
makeLenses ''CreateStorageLocation
makeLenses ''DescribeConfigurationSettings
makeLenses ''ValidateConfigurationSettings
makeLenses ''RestartAppServer
makeLenses ''DescribeEnvironments
makeLenses ''CheckDNSAvailability
makeLenses ''DescribeApplicationVersions
makeLenses ''CreateEnvironment

-- Responses
makeLenses ''DescribeApplicationsResponse
makeLenses ''UpdateEnvironmentResponse
makeLenses ''TerminateEnvironmentResponse
makeLenses ''CreateApplicationVersionResponse
makeLenses ''DescribeEventsResponse
makeLenses ''RequestEnvironmentInfoResponse
makeLenses ''RetrieveEnvironmentInfoResponse
makeLenses ''DeleteApplicationResponse
makeLenses ''UpdateApplicationResponse
makeLenses ''CreateApplicationResponse
makeLenses ''DeleteConfigurationTemplateResponse
makeLenses ''UpdateConfigurationTemplateResponse
makeLenses ''DescribeEnvironmentResourcesResponse
makeLenses ''DeleteApplicationVersionResponse
makeLenses ''UpdateApplicationVersionResponse
makeLenses ''CreateConfigurationTemplateResponse
makeLenses ''RebuildEnvironmentResponse
makeLenses ''DeleteEnvironmentConfigurationResponse
makeLenses ''SwapEnvironmentCNAMEsResponse
makeLenses ''ListAvailableSolutionStacksResponse
makeLenses ''DescribeConfigurationOptionsResponse
makeLenses ''CreateStorageLocationResponse
makeLenses ''DescribeConfigurationSettingsResponse
makeLenses ''ValidateConfigurationSettingsResponse
makeLenses ''RestartAppServerResponse
makeLenses ''DescribeEnvironmentsResponse
makeLenses ''CheckDNSAvailabilityResponse
makeLenses ''DescribeApplicationVersionsResponse
makeLenses ''CreateEnvironmentResponse
