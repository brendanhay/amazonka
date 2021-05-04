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
-- Module      : Network.AWS.Organizations.Types.Root
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Organizations.Types.Root where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Organizations.Types.PolicyTypeSummary
import qualified Network.AWS.Prelude as Prelude

-- | Contains details about a root. A root is a top-level parent node in the
-- hierarchy of an organization that can contain organizational units (OUs)
-- and accounts. The root contains every AWS account in the organization.
--
-- /See:/ 'newRoot' smart constructor.
data Root = Root'
  { -- | The types of policies that are currently enabled for the root and
    -- therefore can be attached to the root or to its OUs or accounts.
    --
    -- Even if a policy type is shown as available in the organization, you can
    -- separately enable and disable them at the root level by using
    -- EnablePolicyType and DisablePolicyType. Use DescribeOrganization to see
    -- the availability of the policy types in that organization.
    policyTypes :: Prelude.Maybe [PolicyTypeSummary],
    -- | The Amazon Resource Name (ARN) of the root.
    --
    -- For more information about ARNs in Organizations, see
    -- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsorganizations.html#awsorganizations-resources-for-iam-policies ARN Formats Supported by Organizations>
    -- in the /AWS Service Authorization Reference/.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier (ID) for the root.
    --
    -- The <http://wikipedia.org/wiki/regex regex pattern> for a root ID string
    -- requires \"r-\" followed by from 4 to 32 lowercase letters or digits.
    id :: Prelude.Maybe Prelude.Text,
    -- | The friendly name of the root.
    --
    -- The <http://wikipedia.org/wiki/regex regex pattern> that is used to
    -- validate this parameter is a string of any of the characters in the
    -- ASCII character range.
    name :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Root' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policyTypes', 'root_policyTypes' - The types of policies that are currently enabled for the root and
-- therefore can be attached to the root or to its OUs or accounts.
--
-- Even if a policy type is shown as available in the organization, you can
-- separately enable and disable them at the root level by using
-- EnablePolicyType and DisablePolicyType. Use DescribeOrganization to see
-- the availability of the policy types in that organization.
--
-- 'arn', 'root_arn' - The Amazon Resource Name (ARN) of the root.
--
-- For more information about ARNs in Organizations, see
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsorganizations.html#awsorganizations-resources-for-iam-policies ARN Formats Supported by Organizations>
-- in the /AWS Service Authorization Reference/.
--
-- 'id', 'root_id' - The unique identifier (ID) for the root.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for a root ID string
-- requires \"r-\" followed by from 4 to 32 lowercase letters or digits.
--
-- 'name', 'root_name' - The friendly name of the root.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> that is used to
-- validate this parameter is a string of any of the characters in the
-- ASCII character range.
newRoot ::
  Root
newRoot =
  Root'
    { policyTypes = Prelude.Nothing,
      arn = Prelude.Nothing,
      id = Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | The types of policies that are currently enabled for the root and
-- therefore can be attached to the root or to its OUs or accounts.
--
-- Even if a policy type is shown as available in the organization, you can
-- separately enable and disable them at the root level by using
-- EnablePolicyType and DisablePolicyType. Use DescribeOrganization to see
-- the availability of the policy types in that organization.
root_policyTypes :: Lens.Lens' Root (Prelude.Maybe [PolicyTypeSummary])
root_policyTypes = Lens.lens (\Root' {policyTypes} -> policyTypes) (\s@Root' {} a -> s {policyTypes = a} :: Root) Prelude.. Lens.mapping Prelude._Coerce

-- | The Amazon Resource Name (ARN) of the root.
--
-- For more information about ARNs in Organizations, see
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsorganizations.html#awsorganizations-resources-for-iam-policies ARN Formats Supported by Organizations>
-- in the /AWS Service Authorization Reference/.
root_arn :: Lens.Lens' Root (Prelude.Maybe Prelude.Text)
root_arn = Lens.lens (\Root' {arn} -> arn) (\s@Root' {} a -> s {arn = a} :: Root)

-- | The unique identifier (ID) for the root.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for a root ID string
-- requires \"r-\" followed by from 4 to 32 lowercase letters or digits.
root_id :: Lens.Lens' Root (Prelude.Maybe Prelude.Text)
root_id = Lens.lens (\Root' {id} -> id) (\s@Root' {} a -> s {id = a} :: Root)

-- | The friendly name of the root.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> that is used to
-- validate this parameter is a string of any of the characters in the
-- ASCII character range.
root_name :: Lens.Lens' Root (Prelude.Maybe Prelude.Text)
root_name = Lens.lens (\Root' {name} -> name) (\s@Root' {} a -> s {name = a} :: Root)

instance Prelude.FromJSON Root where
  parseJSON =
    Prelude.withObject
      "Root"
      ( \x ->
          Root'
            Prelude.<$> ( x Prelude..:? "PolicyTypes"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "Arn")
            Prelude.<*> (x Prelude..:? "Id")
            Prelude.<*> (x Prelude..:? "Name")
      )

instance Prelude.Hashable Root

instance Prelude.NFData Root
