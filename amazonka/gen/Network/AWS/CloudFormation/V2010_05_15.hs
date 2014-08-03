-- Module      : Network.AWS.CloudFormation.V2010_05_15
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
module Network.AWS.CloudFormation.V2010_05_15 (module Export) where

import Network.AWS.CloudFormation.V2010_05_15.CancelUpdateStack as Export
import Network.AWS.CloudFormation.V2010_05_15.CreateStack as Export
import Network.AWS.CloudFormation.V2010_05_15.DeleteStack as Export
import Network.AWS.CloudFormation.V2010_05_15.DescribeStackEvents as Export
import Network.AWS.CloudFormation.V2010_05_15.DescribeStackResource as Export
import Network.AWS.CloudFormation.V2010_05_15.DescribeStackResources as Export
import Network.AWS.CloudFormation.V2010_05_15.DescribeStacks as Export
import Network.AWS.CloudFormation.V2010_05_15.EstimateTemplateCost as Export
import Network.AWS.CloudFormation.V2010_05_15.GetStackPolicy as Export
import Network.AWS.CloudFormation.V2010_05_15.GetTemplate as Export
import Network.AWS.CloudFormation.V2010_05_15.ListStackResources as Export
import Network.AWS.CloudFormation.V2010_05_15.ListStacks as Export
import Network.AWS.CloudFormation.V2010_05_15.SetStackPolicy as Export
import Network.AWS.CloudFormation.V2010_05_15.Types as Export
import Network.AWS.CloudFormation.V2010_05_15.UpdateStack as Export
import Network.AWS.CloudFormation.V2010_05_15.ValidateTemplate as Export
