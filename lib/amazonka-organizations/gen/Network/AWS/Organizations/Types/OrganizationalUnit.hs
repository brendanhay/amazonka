{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.Types.OrganizationalUnit
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Organizations.Types.OrganizationalUnit
  ( OrganizationalUnit (..),

    -- * Smart constructor
    mkOrganizationalUnit,

    -- * Lenses
    ouArn,
    ouId,
    ouName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Organizations.Types.Id as Types
import qualified Network.AWS.Organizations.Types.Name as Types
import qualified Network.AWS.Organizations.Types.OrganizationalUnitArn as Types
import qualified Network.AWS.Prelude as Core

-- | Contains details about an organizational unit (OU). An OU is a container of AWS accounts within a root of an organization. Policies that are attached to an OU apply to all accounts contained in that OU and in any child OUs.
--
-- /See:/ 'mkOrganizationalUnit' smart constructor.
data OrganizationalUnit = OrganizationalUnit'
  { -- | The Amazon Resource Name (ARN) of this OU.
    --
    -- For more information about ARNs in Organizations, see <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_permissions.html#orgs-permissions-arns ARN Formats Supported by Organizations> in the /AWS Organizations User Guide/ .
    arn :: Core.Maybe Types.OrganizationalUnitArn,
    -- | The unique identifier (ID) associated with this OU.
    --
    -- The <http://wikipedia.org/wiki/regex regex pattern> for an organizational unit ID string requires "ou-" followed by from 4 to 32 lowercase letters or digits (the ID of the root that contains the OU). This string is followed by a second "-" dash and from 8 to 32 additional lowercase letters or digits.
    id :: Core.Maybe Types.Id,
    -- | The friendly name of this OU.
    --
    -- The <http://wikipedia.org/wiki/regex regex pattern> that is used to validate this parameter is a string of any of the characters in the ASCII character range.
    name :: Core.Maybe Types.Name
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'OrganizationalUnit' value with any optional fields omitted.
mkOrganizationalUnit ::
  OrganizationalUnit
mkOrganizationalUnit =
  OrganizationalUnit'
    { arn = Core.Nothing,
      id = Core.Nothing,
      name = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of this OU.
--
-- For more information about ARNs in Organizations, see <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_permissions.html#orgs-permissions-arns ARN Formats Supported by Organizations> in the /AWS Organizations User Guide/ .
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ouArn :: Lens.Lens' OrganizationalUnit (Core.Maybe Types.OrganizationalUnitArn)
ouArn = Lens.field @"arn"
{-# DEPRECATED ouArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The unique identifier (ID) associated with this OU.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for an organizational unit ID string requires "ou-" followed by from 4 to 32 lowercase letters or digits (the ID of the root that contains the OU). This string is followed by a second "-" dash and from 8 to 32 additional lowercase letters or digits.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ouId :: Lens.Lens' OrganizationalUnit (Core.Maybe Types.Id)
ouId = Lens.field @"id"
{-# DEPRECATED ouId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The friendly name of this OU.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> that is used to validate this parameter is a string of any of the characters in the ASCII character range.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ouName :: Lens.Lens' OrganizationalUnit (Core.Maybe Types.Name)
ouName = Lens.field @"name"
{-# DEPRECATED ouName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Core.FromJSON OrganizationalUnit where
  parseJSON =
    Core.withObject "OrganizationalUnit" Core.$
      \x ->
        OrganizationalUnit'
          Core.<$> (x Core..:? "Arn")
          Core.<*> (x Core..:? "Id")
          Core.<*> (x Core..:? "Name")
