{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- AWS CloudFormation
--
-- AWS CloudFormation enables you to create and manage AWS infrastructure
-- deployments predictably and repeatedly. AWS CloudFormation helps you
-- leverage AWS products such as Amazon EC2, EBS, Amazon SNS, ELB, and Auto
-- Scaling to build highly-reliable, highly scalable, cost effective
-- applications without worrying about creating and configuring the
-- underlying AWS infrastructure.
--
-- With AWS CloudFormation, you declare all of your resources and
-- dependencies in a template file. The template defines a collection of
-- resources as a single unit called a stack. AWS CloudFormation creates
-- and deletes all member resources of the stack together and manages all
-- dependencies between the resources for you.
--
-- For more information about this product, go to the
-- <http://aws.amazon.com/cloudformation/ CloudFormation Product Page>.
--
-- Amazon CloudFormation makes use of other AWS products. If you need
-- additional technical information about a specific AWS product, you can
-- find the product\'s technical documentation at
-- <http://aws.amazon.com/documentation/>.
module Network.AWS.CloudFormation
    ( module Export
    ) where

import           Network.AWS.CloudFormation.CancelUpdateStack      as Export
import           Network.AWS.CloudFormation.CreateStack            as Export
import           Network.AWS.CloudFormation.DeleteStack            as Export
import           Network.AWS.CloudFormation.DescribeStackEvents    as Export
import           Network.AWS.CloudFormation.DescribeStackResource  as Export
import           Network.AWS.CloudFormation.DescribeStackResources as Export
import           Network.AWS.CloudFormation.DescribeStacks         as Export
import           Network.AWS.CloudFormation.EstimateTemplateCost   as Export
import           Network.AWS.CloudFormation.GetStackPolicy         as Export
import           Network.AWS.CloudFormation.GetTemplate            as Export
import           Network.AWS.CloudFormation.GetTemplateSummary     as Export
import           Network.AWS.CloudFormation.ListStackResources     as Export
import           Network.AWS.CloudFormation.ListStacks             as Export
import           Network.AWS.CloudFormation.SetStackPolicy         as Export
import           Network.AWS.CloudFormation.SignalResource         as Export
import           Network.AWS.CloudFormation.Types                  as Export
import           Network.AWS.CloudFormation.UpdateStack            as Export
import           Network.AWS.CloudFormation.ValidateTemplate       as Export
import           Network.AWS.CloudFormation.Waiters                as Export
