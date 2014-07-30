{-# LANGUAGE TemplateHaskell             #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudFormation.V2010_05_15.Lenses
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.CloudFormation.V2010_05_15.Lenses where

import Control.Lens.TH
import Network.AWS.CloudFormation.V2010_05_15.Types
import Network.AWS.CloudFormation.V2010_05_15.DeleteStack
import Network.AWS.CloudFormation.V2010_05_15.UpdateStack
import Network.AWS.CloudFormation.V2010_05_15.ListStackResources
import Network.AWS.CloudFormation.V2010_05_15.GetStackPolicy
import Network.AWS.CloudFormation.V2010_05_15.DescribeStacks
import Network.AWS.CloudFormation.V2010_05_15.ValidateTemplate
import Network.AWS.CloudFormation.V2010_05_15.CancelUpdateStack
import Network.AWS.CloudFormation.V2010_05_15.DescribeStackEvents
import Network.AWS.CloudFormation.V2010_05_15.SetStackPolicy
import Network.AWS.CloudFormation.V2010_05_15.ListStacks
import Network.AWS.CloudFormation.V2010_05_15.DescribeStackResources
import Network.AWS.CloudFormation.V2010_05_15.CreateStack
import Network.AWS.CloudFormation.V2010_05_15.EstimateTemplateCost
import Network.AWS.CloudFormation.V2010_05_15.GetTemplate
import Network.AWS.CloudFormation.V2010_05_15.DescribeStackResource

-- Newtypes

-- Products
makeLenses ''Output
makeLenses ''Parameter
makeLenses ''Stack
makeLenses ''StackEvent
makeLenses ''StackResource
makeLenses ''StackResourceDetail
makeLenses ''StackResourceSummary
makeLenses ''StackSummary
makeLenses ''Tag
makeLenses ''TemplateParameter

-- Requests
makeLenses ''DeleteStack
makeLenses ''UpdateStack
makeLenses ''ListStackResources
makeLenses ''GetStackPolicy
makeLenses ''DescribeStacks
makeLenses ''ValidateTemplate
makeLenses ''CancelUpdateStack
makeLenses ''DescribeStackEvents
makeLenses ''SetStackPolicy
makeLenses ''ListStacks
makeLenses ''DescribeStackResources
makeLenses ''CreateStack
makeLenses ''EstimateTemplateCost
makeLenses ''GetTemplate
makeLenses ''DescribeStackResource

-- Responses
makeLenses ''DeleteStackResponse
makeLenses ''UpdateStackResponse
makeLenses ''ListStackResourcesResponse
makeLenses ''GetStackPolicyResponse
makeLenses ''DescribeStacksResponse
makeLenses ''ValidateTemplateResponse
makeLenses ''CancelUpdateStackResponse
makeLenses ''DescribeStackEventsResponse
makeLenses ''SetStackPolicyResponse
makeLenses ''ListStacksResponse
makeLenses ''DescribeStackResourcesResponse
makeLenses ''CreateStackResponse
makeLenses ''EstimateTemplateCostResponse
makeLenses ''GetTemplateResponse
makeLenses ''DescribeStackResourceResponse
