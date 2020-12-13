{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.Types.PolicySummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Organizations.Types.PolicySummary
  ( PolicySummary (..),

    -- * Smart constructor
    mkPolicySummary,

    -- * Lenses
    psARN,
    psName,
    psId,
    psAWSManaged,
    psType,
    psDescription,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Organizations.Types.PolicyType
import qualified Network.AWS.Prelude as Lude

-- | Contains information about a policy, but does not include the content. To see the content of a policy, see 'DescribePolicy' .
--
-- /See:/ 'mkPolicySummary' smart constructor.
data PolicySummary = PolicySummary'
  { -- | The Amazon Resource Name (ARN) of the policy.
    --
    -- For more information about ARNs in Organizations, see <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_permissions.html#orgs-permissions-arns ARN Formats Supported by Organizations> in the /AWS Organizations User Guide/ .
    arn :: Lude.Maybe Lude.Text,
    -- | The friendly name of the policy.
    --
    -- The <http://wikipedia.org/wiki/regex regex pattern> that is used to validate this parameter is a string of any of the characters in the ASCII character range.
    name :: Lude.Maybe Lude.Text,
    -- | The unique identifier (ID) of the policy.
    --
    -- The <http://wikipedia.org/wiki/regex regex pattern> for a policy ID string requires "p-" followed by from 8 to 128 lowercase or uppercase letters, digits, or the underscore character (_).
    id :: Lude.Maybe Lude.Text,
    -- | A boolean value that indicates whether the specified policy is an AWS managed policy. If true, then you can attach the policy to roots, OUs, or accounts, but you cannot edit it.
    awsManaged :: Lude.Maybe Lude.Bool,
    -- | The type of policy.
    type' :: Lude.Maybe PolicyType,
    -- | The description of the policy.
    description :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PolicySummary' with the minimum fields required to make a request.
--
-- * 'arn' - The Amazon Resource Name (ARN) of the policy.
--
-- For more information about ARNs in Organizations, see <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_permissions.html#orgs-permissions-arns ARN Formats Supported by Organizations> in the /AWS Organizations User Guide/ .
-- * 'name' - The friendly name of the policy.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> that is used to validate this parameter is a string of any of the characters in the ASCII character range.
-- * 'id' - The unique identifier (ID) of the policy.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for a policy ID string requires "p-" followed by from 8 to 128 lowercase or uppercase letters, digits, or the underscore character (_).
-- * 'awsManaged' - A boolean value that indicates whether the specified policy is an AWS managed policy. If true, then you can attach the policy to roots, OUs, or accounts, but you cannot edit it.
-- * 'type'' - The type of policy.
-- * 'description' - The description of the policy.
mkPolicySummary ::
  PolicySummary
mkPolicySummary =
  PolicySummary'
    { arn = Lude.Nothing,
      name = Lude.Nothing,
      id = Lude.Nothing,
      awsManaged = Lude.Nothing,
      type' = Lude.Nothing,
      description = Lude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the policy.
--
-- For more information about ARNs in Organizations, see <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_permissions.html#orgs-permissions-arns ARN Formats Supported by Organizations> in the /AWS Organizations User Guide/ .
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psARN :: Lens.Lens' PolicySummary (Lude.Maybe Lude.Text)
psARN = Lens.lens (arn :: PolicySummary -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: PolicySummary)
{-# DEPRECATED psARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The friendly name of the policy.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> that is used to validate this parameter is a string of any of the characters in the ASCII character range.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psName :: Lens.Lens' PolicySummary (Lude.Maybe Lude.Text)
psName = Lens.lens (name :: PolicySummary -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: PolicySummary)
{-# DEPRECATED psName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The unique identifier (ID) of the policy.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for a policy ID string requires "p-" followed by from 8 to 128 lowercase or uppercase letters, digits, or the underscore character (_).
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psId :: Lens.Lens' PolicySummary (Lude.Maybe Lude.Text)
psId = Lens.lens (id :: PolicySummary -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: PolicySummary)
{-# DEPRECATED psId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | A boolean value that indicates whether the specified policy is an AWS managed policy. If true, then you can attach the policy to roots, OUs, or accounts, but you cannot edit it.
--
-- /Note:/ Consider using 'awsManaged' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psAWSManaged :: Lens.Lens' PolicySummary (Lude.Maybe Lude.Bool)
psAWSManaged = Lens.lens (awsManaged :: PolicySummary -> Lude.Maybe Lude.Bool) (\s a -> s {awsManaged = a} :: PolicySummary)
{-# DEPRECATED psAWSManaged "Use generic-lens or generic-optics with 'awsManaged' instead." #-}

-- | The type of policy.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psType :: Lens.Lens' PolicySummary (Lude.Maybe PolicyType)
psType = Lens.lens (type' :: PolicySummary -> Lude.Maybe PolicyType) (\s a -> s {type' = a} :: PolicySummary)
{-# DEPRECATED psType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The description of the policy.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psDescription :: Lens.Lens' PolicySummary (Lude.Maybe Lude.Text)
psDescription = Lens.lens (description :: PolicySummary -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: PolicySummary)
{-# DEPRECATED psDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.FromJSON PolicySummary where
  parseJSON =
    Lude.withObject
      "PolicySummary"
      ( \x ->
          PolicySummary'
            Lude.<$> (x Lude..:? "Arn")
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "Id")
            Lude.<*> (x Lude..:? "AwsManaged")
            Lude.<*> (x Lude..:? "Type")
            Lude.<*> (x Lude..:? "Description")
      )
