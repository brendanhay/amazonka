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
    ouARN,
    ouName,
    ouId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains details about an organizational unit (OU). An OU is a container of AWS accounts within a root of an organization. Policies that are attached to an OU apply to all accounts contained in that OU and in any child OUs.
--
-- /See:/ 'mkOrganizationalUnit' smart constructor.
data OrganizationalUnit = OrganizationalUnit'
  { arn ::
      Lude.Maybe Lude.Text,
    name :: Lude.Maybe Lude.Text,
    id :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'OrganizationalUnit' with the minimum fields required to make a request.
--
-- * 'arn' - The Amazon Resource Name (ARN) of this OU.
--
-- For more information about ARNs in Organizations, see <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_permissions.html#orgs-permissions-arns ARN Formats Supported by Organizations> in the /AWS Organizations User Guide/ .
-- * 'id' - The unique identifier (ID) associated with this OU.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for an organizational unit ID string requires "ou-" followed by from 4 to 32 lowercase letters or digits (the ID of the root that contains the OU). This string is followed by a second "-" dash and from 8 to 32 additional lowercase letters or digits.
-- * 'name' - The friendly name of this OU.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> that is used to validate this parameter is a string of any of the characters in the ASCII character range.
mkOrganizationalUnit ::
  OrganizationalUnit
mkOrganizationalUnit =
  OrganizationalUnit'
    { arn = Lude.Nothing,
      name = Lude.Nothing,
      id = Lude.Nothing
    }

-- | The Amazon Resource Name (ARN) of this OU.
--
-- For more information about ARNs in Organizations, see <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_permissions.html#orgs-permissions-arns ARN Formats Supported by Organizations> in the /AWS Organizations User Guide/ .
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ouARN :: Lens.Lens' OrganizationalUnit (Lude.Maybe Lude.Text)
ouARN = Lens.lens (arn :: OrganizationalUnit -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: OrganizationalUnit)
{-# DEPRECATED ouARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The friendly name of this OU.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> that is used to validate this parameter is a string of any of the characters in the ASCII character range.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ouName :: Lens.Lens' OrganizationalUnit (Lude.Maybe Lude.Text)
ouName = Lens.lens (name :: OrganizationalUnit -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: OrganizationalUnit)
{-# DEPRECATED ouName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The unique identifier (ID) associated with this OU.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for an organizational unit ID string requires "ou-" followed by from 4 to 32 lowercase letters or digits (the ID of the root that contains the OU). This string is followed by a second "-" dash and from 8 to 32 additional lowercase letters or digits.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ouId :: Lens.Lens' OrganizationalUnit (Lude.Maybe Lude.Text)
ouId = Lens.lens (id :: OrganizationalUnit -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: OrganizationalUnit)
{-# DEPRECATED ouId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.FromJSON OrganizationalUnit where
  parseJSON =
    Lude.withObject
      "OrganizationalUnit"
      ( \x ->
          OrganizationalUnit'
            Lude.<$> (x Lude..:? "Arn")
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "Id")
      )
