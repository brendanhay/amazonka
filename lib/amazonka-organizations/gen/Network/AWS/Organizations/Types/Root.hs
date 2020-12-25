{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.Types.Root
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Organizations.Types.Root
  ( Root (..),

    -- * Smart constructor
    mkRoot,

    -- * Lenses
    rArn,
    rId,
    rName,
    rPolicyTypes,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Organizations.Types.Arn as Types
import qualified Network.AWS.Organizations.Types.Name as Types
import qualified Network.AWS.Organizations.Types.PolicyTypeSummary as Types
import qualified Network.AWS.Organizations.Types.RootId as Types
import qualified Network.AWS.Prelude as Core

-- | Contains details about a root. A root is a top-level parent node in the hierarchy of an organization that can contain organizational units (OUs) and accounts. The root contains every AWS account in the organization.
--
-- /See:/ 'mkRoot' smart constructor.
data Root = Root'
  { -- | The Amazon Resource Name (ARN) of the root.
    --
    -- For more information about ARNs in Organizations, see <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_permissions.html#orgs-permissions-arns ARN Formats Supported by Organizations> in the /AWS Organizations User Guide/ .
    arn :: Core.Maybe Types.Arn,
    -- | The unique identifier (ID) for the root.
    --
    -- The <http://wikipedia.org/wiki/regex regex pattern> for a root ID string requires "r-" followed by from 4 to 32 lowercase letters or digits.
    id :: Core.Maybe Types.RootId,
    -- | The friendly name of the root.
    --
    -- The <http://wikipedia.org/wiki/regex regex pattern> that is used to validate this parameter is a string of any of the characters in the ASCII character range.
    name :: Core.Maybe Types.Name,
    -- | The types of policies that are currently enabled for the root and therefore can be attached to the root or to its OUs or accounts.
    policyTypes :: Core.Maybe [Types.PolicyTypeSummary]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Root' value with any optional fields omitted.
mkRoot ::
  Root
mkRoot =
  Root'
    { arn = Core.Nothing,
      id = Core.Nothing,
      name = Core.Nothing,
      policyTypes = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the root.
--
-- For more information about ARNs in Organizations, see <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_permissions.html#orgs-permissions-arns ARN Formats Supported by Organizations> in the /AWS Organizations User Guide/ .
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rArn :: Lens.Lens' Root (Core.Maybe Types.Arn)
rArn = Lens.field @"arn"
{-# DEPRECATED rArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The unique identifier (ID) for the root.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for a root ID string requires "r-" followed by from 4 to 32 lowercase letters or digits.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rId :: Lens.Lens' Root (Core.Maybe Types.RootId)
rId = Lens.field @"id"
{-# DEPRECATED rId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The friendly name of the root.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> that is used to validate this parameter is a string of any of the characters in the ASCII character range.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rName :: Lens.Lens' Root (Core.Maybe Types.Name)
rName = Lens.field @"name"
{-# DEPRECATED rName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The types of policies that are currently enabled for the root and therefore can be attached to the root or to its OUs or accounts.
--
-- /Note:/ Consider using 'policyTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rPolicyTypes :: Lens.Lens' Root (Core.Maybe [Types.PolicyTypeSummary])
rPolicyTypes = Lens.field @"policyTypes"
{-# DEPRECATED rPolicyTypes "Use generic-lens or generic-optics with 'policyTypes' instead." #-}

instance Core.FromJSON Root where
  parseJSON =
    Core.withObject "Root" Core.$
      \x ->
        Root'
          Core.<$> (x Core..:? "Arn")
          Core.<*> (x Core..:? "Id")
          Core.<*> (x Core..:? "Name")
          Core.<*> (x Core..:? "PolicyTypes")
