{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.Types.PolicySummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Organizations.Types.PolicySummary
  ( PolicySummary (..)
  -- * Smart constructor
  , mkPolicySummary
  -- * Lenses
  , psArn
  , psAwsManaged
  , psDescription
  , psId
  , psName
  , psType
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Organizations.Types.Arn as Types
import qualified Network.AWS.Organizations.Types.PolicyDescription as Types
import qualified Network.AWS.Organizations.Types.PolicyId as Types
import qualified Network.AWS.Organizations.Types.PolicyName as Types
import qualified Network.AWS.Organizations.Types.PolicyType as Types
import qualified Network.AWS.Prelude as Core

-- | Contains information about a policy, but does not include the content. To see the content of a policy, see 'DescribePolicy' .
--
-- /See:/ 'mkPolicySummary' smart constructor.
data PolicySummary = PolicySummary'
  { arn :: Core.Maybe Types.Arn
    -- ^ The Amazon Resource Name (ARN) of the policy.
--
-- For more information about ARNs in Organizations, see <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_permissions.html#orgs-permissions-arns ARN Formats Supported by Organizations> in the /AWS Organizations User Guide/ .
  , awsManaged :: Core.Maybe Core.Bool
    -- ^ A boolean value that indicates whether the specified policy is an AWS managed policy. If true, then you can attach the policy to roots, OUs, or accounts, but you cannot edit it.
  , description :: Core.Maybe Types.PolicyDescription
    -- ^ The description of the policy.
  , id :: Core.Maybe Types.PolicyId
    -- ^ The unique identifier (ID) of the policy.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for a policy ID string requires "p-" followed by from 8 to 128 lowercase or uppercase letters, digits, or the underscore character (_).
  , name :: Core.Maybe Types.PolicyName
    -- ^ The friendly name of the policy.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> that is used to validate this parameter is a string of any of the characters in the ASCII character range.
  , type' :: Core.Maybe Types.PolicyType
    -- ^ The type of policy.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PolicySummary' value with any optional fields omitted.
mkPolicySummary
    :: PolicySummary
mkPolicySummary
  = PolicySummary'{arn = Core.Nothing, awsManaged = Core.Nothing,
                   description = Core.Nothing, id = Core.Nothing, name = Core.Nothing,
                   type' = Core.Nothing}

-- | The Amazon Resource Name (ARN) of the policy.
--
-- For more information about ARNs in Organizations, see <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_permissions.html#orgs-permissions-arns ARN Formats Supported by Organizations> in the /AWS Organizations User Guide/ .
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psArn :: Lens.Lens' PolicySummary (Core.Maybe Types.Arn)
psArn = Lens.field @"arn"
{-# INLINEABLE psArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | A boolean value that indicates whether the specified policy is an AWS managed policy. If true, then you can attach the policy to roots, OUs, or accounts, but you cannot edit it.
--
-- /Note:/ Consider using 'awsManaged' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psAwsManaged :: Lens.Lens' PolicySummary (Core.Maybe Core.Bool)
psAwsManaged = Lens.field @"awsManaged"
{-# INLINEABLE psAwsManaged #-}
{-# DEPRECATED awsManaged "Use generic-lens or generic-optics with 'awsManaged' instead"  #-}

-- | The description of the policy.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psDescription :: Lens.Lens' PolicySummary (Core.Maybe Types.PolicyDescription)
psDescription = Lens.field @"description"
{-# INLINEABLE psDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The unique identifier (ID) of the policy.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for a policy ID string requires "p-" followed by from 8 to 128 lowercase or uppercase letters, digits, or the underscore character (_).
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psId :: Lens.Lens' PolicySummary (Core.Maybe Types.PolicyId)
psId = Lens.field @"id"
{-# INLINEABLE psId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | The friendly name of the policy.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> that is used to validate this parameter is a string of any of the characters in the ASCII character range.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psName :: Lens.Lens' PolicySummary (Core.Maybe Types.PolicyName)
psName = Lens.field @"name"
{-# INLINEABLE psName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The type of policy.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psType :: Lens.Lens' PolicySummary (Core.Maybe Types.PolicyType)
psType = Lens.field @"type'"
{-# INLINEABLE psType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

instance Core.FromJSON PolicySummary where
        parseJSON
          = Core.withObject "PolicySummary" Core.$
              \ x ->
                PolicySummary' Core.<$>
                  (x Core..:? "Arn") Core.<*> x Core..:? "AwsManaged" Core.<*>
                    x Core..:? "Description"
                    Core.<*> x Core..:? "Id"
                    Core.<*> x Core..:? "Name"
                    Core.<*> x Core..:? "Type"
