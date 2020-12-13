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
    rARN,
    rName,
    rId,
    rPolicyTypes,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Organizations.Types.PolicyTypeSummary
import qualified Network.AWS.Prelude as Lude

-- | Contains details about a root. A root is a top-level parent node in the hierarchy of an organization that can contain organizational units (OUs) and accounts. The root contains every AWS account in the organization.
--
-- /See:/ 'mkRoot' smart constructor.
data Root = Root'
  { -- | The Amazon Resource Name (ARN) of the root.
    --
    -- For more information about ARNs in Organizations, see <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_permissions.html#orgs-permissions-arns ARN Formats Supported by Organizations> in the /AWS Organizations User Guide/ .
    arn :: Lude.Maybe Lude.Text,
    -- | The friendly name of the root.
    --
    -- The <http://wikipedia.org/wiki/regex regex pattern> that is used to validate this parameter is a string of any of the characters in the ASCII character range.
    name :: Lude.Maybe Lude.Text,
    -- | The unique identifier (ID) for the root.
    --
    -- The <http://wikipedia.org/wiki/regex regex pattern> for a root ID string requires "r-" followed by from 4 to 32 lowercase letters or digits.
    id :: Lude.Maybe Lude.Text,
    -- | The types of policies that are currently enabled for the root and therefore can be attached to the root or to its OUs or accounts.
    policyTypes :: Lude.Maybe [PolicyTypeSummary]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Root' with the minimum fields required to make a request.
--
-- * 'arn' - The Amazon Resource Name (ARN) of the root.
--
-- For more information about ARNs in Organizations, see <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_permissions.html#orgs-permissions-arns ARN Formats Supported by Organizations> in the /AWS Organizations User Guide/ .
-- * 'name' - The friendly name of the root.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> that is used to validate this parameter is a string of any of the characters in the ASCII character range.
-- * 'id' - The unique identifier (ID) for the root.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for a root ID string requires "r-" followed by from 4 to 32 lowercase letters or digits.
-- * 'policyTypes' - The types of policies that are currently enabled for the root and therefore can be attached to the root or to its OUs or accounts.
mkRoot ::
  Root
mkRoot =
  Root'
    { arn = Lude.Nothing,
      name = Lude.Nothing,
      id = Lude.Nothing,
      policyTypes = Lude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the root.
--
-- For more information about ARNs in Organizations, see <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_permissions.html#orgs-permissions-arns ARN Formats Supported by Organizations> in the /AWS Organizations User Guide/ .
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rARN :: Lens.Lens' Root (Lude.Maybe Lude.Text)
rARN = Lens.lens (arn :: Root -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: Root)
{-# DEPRECATED rARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The friendly name of the root.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> that is used to validate this parameter is a string of any of the characters in the ASCII character range.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rName :: Lens.Lens' Root (Lude.Maybe Lude.Text)
rName = Lens.lens (name :: Root -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: Root)
{-# DEPRECATED rName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The unique identifier (ID) for the root.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for a root ID string requires "r-" followed by from 4 to 32 lowercase letters or digits.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rId :: Lens.Lens' Root (Lude.Maybe Lude.Text)
rId = Lens.lens (id :: Root -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: Root)
{-# DEPRECATED rId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The types of policies that are currently enabled for the root and therefore can be attached to the root or to its OUs or accounts.
--
-- /Note:/ Consider using 'policyTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rPolicyTypes :: Lens.Lens' Root (Lude.Maybe [PolicyTypeSummary])
rPolicyTypes = Lens.lens (policyTypes :: Root -> Lude.Maybe [PolicyTypeSummary]) (\s a -> s {policyTypes = a} :: Root)
{-# DEPRECATED rPolicyTypes "Use generic-lens or generic-optics with 'policyTypes' instead." #-}

instance Lude.FromJSON Root where
  parseJSON =
    Lude.withObject
      "Root"
      ( \x ->
          Root'
            Lude.<$> (x Lude..:? "Arn")
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "Id")
            Lude.<*> (x Lude..:? "PolicyTypes" Lude..!= Lude.mempty)
      )
