{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.Types.PolicyTargetSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Organizations.Types.PolicyTargetSummary
  ( PolicyTargetSummary (..),

    -- * Smart constructor
    mkPolicyTargetSummary,

    -- * Lenses
    ptsfTargetId,
    ptsfARN,
    ptsfName,
    ptsfType,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Organizations.Types.TargetType
import qualified Network.AWS.Prelude as Lude

-- | Contains information about a root, OU, or account that a policy is attached to.
--
-- /See:/ 'mkPolicyTargetSummary' smart constructor.
data PolicyTargetSummary = PolicyTargetSummary'
  { -- | The unique identifier (ID) of the policy target.
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
    targetId :: Lude.Maybe Lude.Text,
    -- | The Amazon Resource Name (ARN) of the policy target.
    --
    -- For more information about ARNs in Organizations, see <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_permissions.html#orgs-permissions-arns ARN Formats Supported by Organizations> in the /AWS Organizations User Guide/ .
    arn :: Lude.Maybe Lude.Text,
    -- | The friendly name of the policy target.
    --
    -- The <http://wikipedia.org/wiki/regex regex pattern> that is used to validate this parameter is a string of any of the characters in the ASCII character range.
    name :: Lude.Maybe Lude.Text,
    -- | The type of the policy target.
    type' :: Lude.Maybe TargetType
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PolicyTargetSummary' with the minimum fields required to make a request.
--
-- * 'targetId' - The unique identifier (ID) of the policy target.
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
-- * 'arn' - The Amazon Resource Name (ARN) of the policy target.
--
-- For more information about ARNs in Organizations, see <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_permissions.html#orgs-permissions-arns ARN Formats Supported by Organizations> in the /AWS Organizations User Guide/ .
-- * 'name' - The friendly name of the policy target.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> that is used to validate this parameter is a string of any of the characters in the ASCII character range.
-- * 'type'' - The type of the policy target.
mkPolicyTargetSummary ::
  PolicyTargetSummary
mkPolicyTargetSummary =
  PolicyTargetSummary'
    { targetId = Lude.Nothing,
      arn = Lude.Nothing,
      name = Lude.Nothing,
      type' = Lude.Nothing
    }

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
ptsfTargetId :: Lens.Lens' PolicyTargetSummary (Lude.Maybe Lude.Text)
ptsfTargetId = Lens.lens (targetId :: PolicyTargetSummary -> Lude.Maybe Lude.Text) (\s a -> s {targetId = a} :: PolicyTargetSummary)
{-# DEPRECATED ptsfTargetId "Use generic-lens or generic-optics with 'targetId' instead." #-}

-- | The Amazon Resource Name (ARN) of the policy target.
--
-- For more information about ARNs in Organizations, see <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_permissions.html#orgs-permissions-arns ARN Formats Supported by Organizations> in the /AWS Organizations User Guide/ .
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptsfARN :: Lens.Lens' PolicyTargetSummary (Lude.Maybe Lude.Text)
ptsfARN = Lens.lens (arn :: PolicyTargetSummary -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: PolicyTargetSummary)
{-# DEPRECATED ptsfARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The friendly name of the policy target.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> that is used to validate this parameter is a string of any of the characters in the ASCII character range.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptsfName :: Lens.Lens' PolicyTargetSummary (Lude.Maybe Lude.Text)
ptsfName = Lens.lens (name :: PolicyTargetSummary -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: PolicyTargetSummary)
{-# DEPRECATED ptsfName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The type of the policy target.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptsfType :: Lens.Lens' PolicyTargetSummary (Lude.Maybe TargetType)
ptsfType = Lens.lens (type' :: PolicyTargetSummary -> Lude.Maybe TargetType) (\s a -> s {type' = a} :: PolicyTargetSummary)
{-# DEPRECATED ptsfType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Lude.FromJSON PolicyTargetSummary where
  parseJSON =
    Lude.withObject
      "PolicyTargetSummary"
      ( \x ->
          PolicyTargetSummary'
            Lude.<$> (x Lude..:? "TargetId")
            Lude.<*> (x Lude..:? "Arn")
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "Type")
      )
