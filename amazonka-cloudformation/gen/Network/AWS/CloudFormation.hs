-- Module      : Network.AWS.CloudFormation
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | AWS CloudFormation gives developers and systems administrators an easy way
-- to create and manage a collection of related AWS resources, provisioning
-- and updating them in an orderly and predictable fashion. You can use AWS
-- CloudFormation’s sample templates or create your own templates to describe
-- the AWS resources, and any associated dependencies or runtime parameters,
-- required to run your application. You don’t need to figure out the order
-- for provisioning AWS services or the subtleties of making those
-- dependencies work. CloudFormation takes care of this for you. After the AWS
-- resources are deployed, you can modify and update them in a controlled and
-- predictable way, in effect applying version control to your AWS
-- infrastructure the same way you do with your software.
module Network.AWS.CloudFormation
    ( module Network.AWS.CloudFormation.CancelUpdateStack
    , module Network.AWS.CloudFormation.CreateStack
    , module Network.AWS.CloudFormation.DeleteStack
    , module Network.AWS.CloudFormation.DescribeStackEvents
    , module Network.AWS.CloudFormation.DescribeStackResource
    , module Network.AWS.CloudFormation.DescribeStackResources
    , module Network.AWS.CloudFormation.DescribeStacks
    , module Network.AWS.CloudFormation.EstimateTemplateCost
    , module Network.AWS.CloudFormation.GetStackPolicy
    , module Network.AWS.CloudFormation.GetTemplate
    , module Network.AWS.CloudFormation.ListStackResources
    , module Network.AWS.CloudFormation.ListStacks
    , module Network.AWS.CloudFormation.SetStackPolicy
    , module Network.AWS.CloudFormation.Types
    , module Network.AWS.CloudFormation.UpdateStack
    , module Network.AWS.CloudFormation.ValidateTemplate
    ) where

import Network.AWS.CloudFormation.CancelUpdateStack
import Network.AWS.CloudFormation.CreateStack
import Network.AWS.CloudFormation.DeleteStack
import Network.AWS.CloudFormation.DescribeStackEvents
import Network.AWS.CloudFormation.DescribeStackResource
import Network.AWS.CloudFormation.DescribeStackResources
import Network.AWS.CloudFormation.DescribeStacks
import Network.AWS.CloudFormation.EstimateTemplateCost
import Network.AWS.CloudFormation.GetStackPolicy
import Network.AWS.CloudFormation.GetTemplate
import Network.AWS.CloudFormation.ListStackResources
import Network.AWS.CloudFormation.ListStacks
import Network.AWS.CloudFormation.SetStackPolicy
import Network.AWS.CloudFormation.Types
import Network.AWS.CloudFormation.UpdateStack
import Network.AWS.CloudFormation.ValidateTemplate
