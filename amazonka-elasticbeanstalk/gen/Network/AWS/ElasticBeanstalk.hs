-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- | AWS Elastic Beanstalk
--
-- This is the AWS Elastic Beanstalk API Reference. This guide provides
-- detailed information about AWS Elastic Beanstalk actions, data types,
-- parameters, and errors.
--
-- AWS Elastic Beanstalk is a tool that makes it easy for you to create,
-- deploy, and manage scalable, fault-tolerant applications running on
-- Amazon Web Services cloud resources.
--
-- For more information about this product, go to the
-- <http://aws.amazon.com/elasticbeanstalk/ AWS Elastic Beanstalk> details
-- page. The location of the latest AWS Elastic Beanstalk WSDL is
-- <http://elasticbeanstalk.s3.amazonaws.com/doc/2010-12-01/AWSElasticBeanstalk.wsdl>.
-- To install the Software Development Kits (SDKs), Integrated Development
-- Environment (IDE) Toolkits, and command line tools that enable you to
-- access the API, go to
-- <https://aws.amazon.com/tools/ Tools for Amazon Web Services>.
--
-- __Endpoints__
--
-- For a list of region-specific endpoints that AWS Elastic Beanstalk
-- supports, go to
-- <http://docs.aws.amazon.com/general/latest/gr/rande.html#elasticbeanstalk_region Regions and Endpoints>
-- in the /Amazon Web Services Glossary/.
module Network.AWS.ElasticBeanstalk
    ( module Export
    ) where

import           Network.AWS.ElasticBeanstalk.AbortEnvironmentUpdate         as Export
import           Network.AWS.ElasticBeanstalk.CheckDNSAvailability           as Export
import           Network.AWS.ElasticBeanstalk.CreateApplication              as Export
import           Network.AWS.ElasticBeanstalk.CreateApplicationVersion       as Export
import           Network.AWS.ElasticBeanstalk.CreateConfigurationTemplate    as Export
import           Network.AWS.ElasticBeanstalk.CreateEnvironment              as Export
import           Network.AWS.ElasticBeanstalk.CreateStorageLocation          as Export
import           Network.AWS.ElasticBeanstalk.DeleteApplication              as Export
import           Network.AWS.ElasticBeanstalk.DeleteApplicationVersion       as Export
import           Network.AWS.ElasticBeanstalk.DeleteConfigurationTemplate    as Export
import           Network.AWS.ElasticBeanstalk.DeleteEnvironmentConfiguration as Export
import           Network.AWS.ElasticBeanstalk.DescribeApplications           as Export
import           Network.AWS.ElasticBeanstalk.DescribeApplicationVersions    as Export
import           Network.AWS.ElasticBeanstalk.DescribeConfigurationOptions   as Export
import           Network.AWS.ElasticBeanstalk.DescribeConfigurationSettings  as Export
import           Network.AWS.ElasticBeanstalk.DescribeEnvironmentResources   as Export
import           Network.AWS.ElasticBeanstalk.DescribeEnvironments           as Export
import           Network.AWS.ElasticBeanstalk.DescribeEvents                 as Export
import           Network.AWS.ElasticBeanstalk.ListAvailableSolutionStacks    as Export
import           Network.AWS.ElasticBeanstalk.RebuildEnvironment             as Export
import           Network.AWS.ElasticBeanstalk.RequestEnvironmentInfo         as Export
import           Network.AWS.ElasticBeanstalk.RestartAppServer               as Export
import           Network.AWS.ElasticBeanstalk.RetrieveEnvironmentInfo        as Export
import           Network.AWS.ElasticBeanstalk.SwapEnvironmentCNAMEs          as Export
import           Network.AWS.ElasticBeanstalk.TerminateEnvironment           as Export
import           Network.AWS.ElasticBeanstalk.Types                          as Export
import           Network.AWS.ElasticBeanstalk.UpdateApplication              as Export
import           Network.AWS.ElasticBeanstalk.UpdateApplicationVersion       as Export
import           Network.AWS.ElasticBeanstalk.UpdateConfigurationTemplate    as Export
import           Network.AWS.ElasticBeanstalk.UpdateEnvironment              as Export
import           Network.AWS.ElasticBeanstalk.ValidateConfigurationSettings  as Export
import           Network.AWS.ElasticBeanstalk.Waiters                        as Export
