{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- AWS CodeDeploy __Overview__
--
-- This is the AWS CodeDeploy API Reference. This guide provides
-- descriptions of the AWS CodeDeploy APIs. For additional information, see
-- the
-- <http://docs.aws.amazon.com/codedeploy/latest/userguide AWS CodeDeploy User Guide>.
--
-- __Using the APIs__
--
-- You can use the AWS CodeDeploy APIs to work with the following items:
--
-- -   Applications are unique identifiers that AWS CodeDeploy uses to
--     ensure that the correct combinations of revisions, deployment
--     configurations, and deployment groups are being referenced during
--     deployments.
--
--     You can use the AWS CodeDeploy APIs to create, delete, get, list,
--     and update applications.
--
-- -   Deployment configurations are sets of deployment rules and
--     deployment success and failure conditions that AWS CodeDeploy uses
--     during deployments.
--
--     You can use the AWS CodeDeploy APIs to create, delete, get, and list
--     deployment configurations.
--
-- -   Deployment groups are groups of instances to which application
--     revisions can be deployed.
--
--     You can use the AWS CodeDeploy APIs to create, delete, get, list,
--     and update deployment groups.
--
-- -   Instances represent Amazon EC2 instances to which application
--     revisions are deployed. Instances are identified by their Amazon EC2
--     tags or Auto Scaling group names. Instances belong to deployment
--     groups.
--
--     You can use the AWS CodeDeploy APIs to get and list instances.
--
-- -   Deployments represent the process of deploying revisions to
--     instances.
--
--     You can use the AWS CodeDeploy APIs to create, get, list, and stop
--     deployments.
--
-- -   Application revisions are archive files that are stored in Amazon S3
--     buckets or GitHub repositories. These revisions contain source
--     content (such as source code, web pages, executable files, any
--     deployment scripts, and similar) along with an Application
--     Specification file (AppSpec file). (The AppSpec file is unique to
--     AWS CodeDeploy; it defines a series of deployment actions that you
--     want AWS CodeDeploy to execute.) An application revision is uniquely
--     identified by its Amazon S3 object key and its ETag, version, or
--     both (for application revisions that are stored in Amazon S3
--     buckets) or by its repository name and commit ID (for applications
--     revisions that are stored in GitHub repositories). Application
--     revisions are deployed through deployment groups.
--
--     You can use the AWS CodeDeploy APIs to get, list, and register
--     application revisions.
--
module Network.AWS.CodeDeploy
    ( module Export
    ) where

import           Network.AWS.CodeDeploy.AddTagsToOnPremisesInstances      as Export
import           Network.AWS.CodeDeploy.BatchGetApplications              as Export
import           Network.AWS.CodeDeploy.BatchGetDeployments               as Export
import           Network.AWS.CodeDeploy.BatchGetOnPremisesInstances       as Export
import           Network.AWS.CodeDeploy.CreateApplication                 as Export
import           Network.AWS.CodeDeploy.CreateDeployment                  as Export
import           Network.AWS.CodeDeploy.CreateDeploymentConfig            as Export
import           Network.AWS.CodeDeploy.CreateDeploymentGroup             as Export
import           Network.AWS.CodeDeploy.DeleteApplication                 as Export
import           Network.AWS.CodeDeploy.DeleteDeploymentConfig            as Export
import           Network.AWS.CodeDeploy.DeleteDeploymentGroup             as Export
import           Network.AWS.CodeDeploy.DeregisterOnPremisesInstance      as Export
import           Network.AWS.CodeDeploy.GetApplication                    as Export
import           Network.AWS.CodeDeploy.GetApplicationRevision            as Export
import           Network.AWS.CodeDeploy.GetDeployment                     as Export
import           Network.AWS.CodeDeploy.GetDeploymentConfig               as Export
import           Network.AWS.CodeDeploy.GetDeploymentGroup                as Export
import           Network.AWS.CodeDeploy.GetDeploymentInstance             as Export
import           Network.AWS.CodeDeploy.GetOnPremisesInstance             as Export
import           Network.AWS.CodeDeploy.ListApplicationRevisions          as Export
import           Network.AWS.CodeDeploy.ListApplications                  as Export
import           Network.AWS.CodeDeploy.ListDeploymentConfigs             as Export
import           Network.AWS.CodeDeploy.ListDeploymentGroups              as Export
import           Network.AWS.CodeDeploy.ListDeploymentInstances           as Export
import           Network.AWS.CodeDeploy.ListDeployments                   as Export
import           Network.AWS.CodeDeploy.ListOnPremisesInstances           as Export
import           Network.AWS.CodeDeploy.RegisterApplicationRevision       as Export
import           Network.AWS.CodeDeploy.RegisterOnPremisesInstance        as Export
import           Network.AWS.CodeDeploy.RemoveTagsFromOnPremisesInstances as Export
import           Network.AWS.CodeDeploy.StopDeployment                    as Export
import           Network.AWS.CodeDeploy.Types                             as Export
import           Network.AWS.CodeDeploy.Types.Product                     as Export
import           Network.AWS.CodeDeploy.Types.Sum                         as Export
import           Network.AWS.CodeDeploy.UpdateApplication                 as Export
import           Network.AWS.CodeDeploy.UpdateDeploymentGroup             as Export
import           Network.AWS.CodeDeploy.Waiters                           as Export
