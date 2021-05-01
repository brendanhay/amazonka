{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.Types.PolicySummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Organizations.Types.PolicySummary where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Organizations.Types.PolicyType
import qualified Network.AWS.Prelude as Prelude

-- | Contains information about a policy, but does not include the content.
-- To see the content of a policy, see DescribePolicy.
--
-- /See:/ 'newPolicySummary' smart constructor.
data PolicySummary = PolicySummary'
  { -- | The Amazon Resource Name (ARN) of the policy.
    --
    -- For more information about ARNs in Organizations, see
    -- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsorganizations.html#awsorganizations-resources-for-iam-policies ARN Formats Supported by Organizations>
    -- in the /AWS Service Authorization Reference/.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier (ID) of the policy.
    --
    -- The <http://wikipedia.org/wiki/regex regex pattern> for a policy ID
    -- string requires \"p-\" followed by from 8 to 128 lowercase or uppercase
    -- letters, digits, or the underscore character (_).
    id :: Prelude.Maybe Prelude.Text,
    -- | The friendly name of the policy.
    --
    -- The <http://wikipedia.org/wiki/regex regex pattern> that is used to
    -- validate this parameter is a string of any of the characters in the
    -- ASCII character range.
    name :: Prelude.Maybe Prelude.Text,
    -- | The description of the policy.
    description :: Prelude.Maybe Prelude.Text,
    -- | The type of policy.
    type' :: Prelude.Maybe PolicyType,
    -- | A boolean value that indicates whether the specified policy is an AWS
    -- managed policy. If true, then you can attach the policy to roots, OUs,
    -- or accounts, but you cannot edit it.
    awsManaged :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'PolicySummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'policySummary_arn' - The Amazon Resource Name (ARN) of the policy.
--
-- For more information about ARNs in Organizations, see
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsorganizations.html#awsorganizations-resources-for-iam-policies ARN Formats Supported by Organizations>
-- in the /AWS Service Authorization Reference/.
--
-- 'id', 'policySummary_id' - The unique identifier (ID) of the policy.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for a policy ID
-- string requires \"p-\" followed by from 8 to 128 lowercase or uppercase
-- letters, digits, or the underscore character (_).
--
-- 'name', 'policySummary_name' - The friendly name of the policy.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> that is used to
-- validate this parameter is a string of any of the characters in the
-- ASCII character range.
--
-- 'description', 'policySummary_description' - The description of the policy.
--
-- 'type'', 'policySummary_type' - The type of policy.
--
-- 'awsManaged', 'policySummary_awsManaged' - A boolean value that indicates whether the specified policy is an AWS
-- managed policy. If true, then you can attach the policy to roots, OUs,
-- or accounts, but you cannot edit it.
newPolicySummary ::
  PolicySummary
newPolicySummary =
  PolicySummary'
    { arn = Prelude.Nothing,
      id = Prelude.Nothing,
      name = Prelude.Nothing,
      description = Prelude.Nothing,
      type' = Prelude.Nothing,
      awsManaged = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the policy.
--
-- For more information about ARNs in Organizations, see
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsorganizations.html#awsorganizations-resources-for-iam-policies ARN Formats Supported by Organizations>
-- in the /AWS Service Authorization Reference/.
policySummary_arn :: Lens.Lens' PolicySummary (Prelude.Maybe Prelude.Text)
policySummary_arn = Lens.lens (\PolicySummary' {arn} -> arn) (\s@PolicySummary' {} a -> s {arn = a} :: PolicySummary)

-- | The unique identifier (ID) of the policy.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for a policy ID
-- string requires \"p-\" followed by from 8 to 128 lowercase or uppercase
-- letters, digits, or the underscore character (_).
policySummary_id :: Lens.Lens' PolicySummary (Prelude.Maybe Prelude.Text)
policySummary_id = Lens.lens (\PolicySummary' {id} -> id) (\s@PolicySummary' {} a -> s {id = a} :: PolicySummary)

-- | The friendly name of the policy.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> that is used to
-- validate this parameter is a string of any of the characters in the
-- ASCII character range.
policySummary_name :: Lens.Lens' PolicySummary (Prelude.Maybe Prelude.Text)
policySummary_name = Lens.lens (\PolicySummary' {name} -> name) (\s@PolicySummary' {} a -> s {name = a} :: PolicySummary)

-- | The description of the policy.
policySummary_description :: Lens.Lens' PolicySummary (Prelude.Maybe Prelude.Text)
policySummary_description = Lens.lens (\PolicySummary' {description} -> description) (\s@PolicySummary' {} a -> s {description = a} :: PolicySummary)

-- | The type of policy.
policySummary_type :: Lens.Lens' PolicySummary (Prelude.Maybe PolicyType)
policySummary_type = Lens.lens (\PolicySummary' {type'} -> type') (\s@PolicySummary' {} a -> s {type' = a} :: PolicySummary)

-- | A boolean value that indicates whether the specified policy is an AWS
-- managed policy. If true, then you can attach the policy to roots, OUs,
-- or accounts, but you cannot edit it.
policySummary_awsManaged :: Lens.Lens' PolicySummary (Prelude.Maybe Prelude.Bool)
policySummary_awsManaged = Lens.lens (\PolicySummary' {awsManaged} -> awsManaged) (\s@PolicySummary' {} a -> s {awsManaged = a} :: PolicySummary)

instance Prelude.FromJSON PolicySummary where
  parseJSON =
    Prelude.withObject
      "PolicySummary"
      ( \x ->
          PolicySummary'
            Prelude.<$> (x Prelude..:? "Arn")
            Prelude.<*> (x Prelude..:? "Id")
            Prelude.<*> (x Prelude..:? "Name")
            Prelude.<*> (x Prelude..:? "Description")
            Prelude.<*> (x Prelude..:? "Type")
            Prelude.<*> (x Prelude..:? "AwsManaged")
      )

instance Prelude.Hashable PolicySummary

instance Prelude.NFData PolicySummary
