-- Module      : Network.AWS.CodeDeploy
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Amazon CodeDeploy is a service that automates code deployments to Amazon
-- EC2 instances. Amazon CodeDeploy makes it easier for you to rapidly release
-- new features, helps you avoid downtime during deployment, and handles the
-- complexity of updating your applications. You can use Amazon CodeDeploy to
-- automate deployments, eliminating the need for error-prone manual
-- operations, and the service scales with your infrastructure so you can
-- easily deploy to one EC2 instance or thousands.
module Network.AWS.CodeDeploy
    ( module Network.AWS.CodeDeploy.BatchGetApplications
    , module Network.AWS.CodeDeploy.BatchGetDeployments
    , module Network.AWS.CodeDeploy.CreateApplication
    , module Network.AWS.CodeDeploy.CreateDeployment
    , module Network.AWS.CodeDeploy.CreateDeploymentConfig
    , module Network.AWS.CodeDeploy.CreateDeploymentGroup
    , module Network.AWS.CodeDeploy.DeleteApplication
    , module Network.AWS.CodeDeploy.DeleteDeploymentConfig
    , module Network.AWS.CodeDeploy.DeleteDeploymentGroup
    , module Network.AWS.CodeDeploy.GetApplication
    , module Network.AWS.CodeDeploy.GetApplicationRevision
    , module Network.AWS.CodeDeploy.GetDeployment
    , module Network.AWS.CodeDeploy.GetDeploymentConfig
    , module Network.AWS.CodeDeploy.GetDeploymentGroup
    , module Network.AWS.CodeDeploy.GetDeploymentInstance
    , module Network.AWS.CodeDeploy.ListApplicationRevisions
    , module Network.AWS.CodeDeploy.ListApplications
    , module Network.AWS.CodeDeploy.ListDeploymentConfigs
    , module Network.AWS.CodeDeploy.ListDeploymentGroups
    , module Network.AWS.CodeDeploy.ListDeploymentInstances
    , module Network.AWS.CodeDeploy.ListDeployments
    , module Network.AWS.CodeDeploy.RegisterApplicationRevision
    , module Network.AWS.CodeDeploy.StopDeployment
    , module Network.AWS.CodeDeploy.Types
    , module Network.AWS.CodeDeploy.UpdateApplication
    , module Network.AWS.CodeDeploy.UpdateDeploymentGroup
    ) where

import Network.AWS.CodeDeploy.BatchGetApplications
import Network.AWS.CodeDeploy.BatchGetDeployments
import Network.AWS.CodeDeploy.CreateApplication
import Network.AWS.CodeDeploy.CreateDeployment
import Network.AWS.CodeDeploy.CreateDeploymentConfig
import Network.AWS.CodeDeploy.CreateDeploymentGroup
import Network.AWS.CodeDeploy.DeleteApplication
import Network.AWS.CodeDeploy.DeleteDeploymentConfig
import Network.AWS.CodeDeploy.DeleteDeploymentGroup
import Network.AWS.CodeDeploy.GetApplication
import Network.AWS.CodeDeploy.GetApplicationRevision
import Network.AWS.CodeDeploy.GetDeployment
import Network.AWS.CodeDeploy.GetDeploymentConfig
import Network.AWS.CodeDeploy.GetDeploymentGroup
import Network.AWS.CodeDeploy.GetDeploymentInstance
import Network.AWS.CodeDeploy.ListApplicationRevisions
import Network.AWS.CodeDeploy.ListApplications
import Network.AWS.CodeDeploy.ListDeploymentConfigs
import Network.AWS.CodeDeploy.ListDeploymentGroups
import Network.AWS.CodeDeploy.ListDeploymentInstances
import Network.AWS.CodeDeploy.ListDeployments
import Network.AWS.CodeDeploy.RegisterApplicationRevision
import Network.AWS.CodeDeploy.StopDeployment
import Network.AWS.CodeDeploy.Types
import Network.AWS.CodeDeploy.UpdateApplication
import Network.AWS.CodeDeploy.UpdateDeploymentGroup
