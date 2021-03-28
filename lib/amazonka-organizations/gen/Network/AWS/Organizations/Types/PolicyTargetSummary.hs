{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.Types.PolicyTargetSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Organizations.Types.PolicyTargetSummary
  ( PolicyTargetSummary (..)
  -- * Smart constructor
  , mkPolicyTargetSummary
  -- * Lenses
  , ptsfArn
  , ptsfName
  , ptsfTargetId
  , ptsfType
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Organizations.Types.GenericArn as Types
import qualified Network.AWS.Organizations.Types.Name as Types
import qualified Network.AWS.Organizations.Types.PolicyTargetId as Types
import qualified Network.AWS.Organizations.Types.TargetType as Types
import qualified Network.AWS.Prelude as Core

-- | Contains information about a root, OU, or account that a policy is attached to.
--
-- /See:/ 'mkPolicyTargetSummary' smart constructor.
data PolicyTargetSummary = PolicyTargetSummary'
  { arn :: Core.Maybe Types.GenericArn
    -- ^ The Amazon Resource Name (ARN) of the policy target.
--
-- For more information about ARNs in Organizations, see <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_permissions.html#orgs-permissions-arns ARN Formats Supported by Organizations> in the /AWS Organizations User Guide/ .
  , name :: Core.Maybe Types.Name
    -- ^ The friendly name of the policy target.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> that is used to validate this parameter is a string of any of the characters in the ASCII character range.
  , targetId :: Core.Maybe Types.PolicyTargetId
    -- ^ The unique identifier (ID) of the policy target.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for a target ID string requires one of the following:
--
--     * __Root__ - A string that begins with "r-" followed by from 4 to 32 lowercase letters or digits.
--
--
--     * __Account__ - A string that consists of exactly 12 digits.
--
--
--     * __Organizational unit (OU)__ - A string that begins with "ou-" followed by from 4 to 32 lowercase letters or digits (the ID of the root that the OU is in). This string is followed by a second "-" dash and from 8 to 32 additional lowercase letters or digits.
--
--
  , type' :: Core.Maybe Types.TargetType
    -- ^ The type of the policy target.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PolicyTargetSummary' value with any optional fields omitted.
mkPolicyTargetSummary
    :: PolicyTargetSummary
mkPolicyTargetSummary
  = PolicyTargetSummary'{arn = Core.Nothing, name = Core.Nothing,
                         targetId = Core.Nothing, type' = Core.Nothing}

-- | The Amazon Resource Name (ARN) of the policy target.
--
-- For more information about ARNs in Organizations, see <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_permissions.html#orgs-permissions-arns ARN Formats Supported by Organizations> in the /AWS Organizations User Guide/ .
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptsfArn :: Lens.Lens' PolicyTargetSummary (Core.Maybe Types.GenericArn)
ptsfArn = Lens.field @"arn"
{-# INLINEABLE ptsfArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | The friendly name of the policy target.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> that is used to validate this parameter is a string of any of the characters in the ASCII character range.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptsfName :: Lens.Lens' PolicyTargetSummary (Core.Maybe Types.Name)
ptsfName = Lens.field @"name"
{-# INLINEABLE ptsfName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The unique identifier (ID) of the policy target.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for a target ID string requires one of the following:
--
--     * __Root__ - A string that begins with "r-" followed by from 4 to 32 lowercase letters or digits.
--
--
--     * __Account__ - A string that consists of exactly 12 digits.
--
--
--     * __Organizational unit (OU)__ - A string that begins with "ou-" followed by from 4 to 32 lowercase letters or digits (the ID of the root that the OU is in). This string is followed by a second "-" dash and from 8 to 32 additional lowercase letters or digits.
--
--
--
-- /Note:/ Consider using 'targetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptsfTargetId :: Lens.Lens' PolicyTargetSummary (Core.Maybe Types.PolicyTargetId)
ptsfTargetId = Lens.field @"targetId"
{-# INLINEABLE ptsfTargetId #-}
{-# DEPRECATED targetId "Use generic-lens or generic-optics with 'targetId' instead"  #-}

-- | The type of the policy target.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptsfType :: Lens.Lens' PolicyTargetSummary (Core.Maybe Types.TargetType)
ptsfType = Lens.field @"type'"
{-# INLINEABLE ptsfType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

instance Core.FromJSON PolicyTargetSummary where
        parseJSON
          = Core.withObject "PolicyTargetSummary" Core.$
              \ x ->
                PolicyTargetSummary' Core.<$>
                  (x Core..:? "Arn") Core.<*> x Core..:? "Name" Core.<*>
                    x Core..:? "TargetId"
                    Core.<*> x Core..:? "Type"
