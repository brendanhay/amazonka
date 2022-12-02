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
-- Module      : Amazonka.SSOAdmin.Types.CustomerManagedPolicyReference
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSOAdmin.Types.CustomerManagedPolicyReference where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies the name and path of a customer managed policy. You must have
-- an IAM policy that matches the name and path in each AWS account where
-- you want to deploy your permission set.
--
-- /See:/ 'newCustomerManagedPolicyReference' smart constructor.
data CustomerManagedPolicyReference = CustomerManagedPolicyReference'
  { -- | The path to the IAM policy that you have configured in each account
    -- where you want to deploy your permission set. The default is @\/@. For
    -- more information, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_identifiers.html#identifiers-friendly-names Friendly names and paths>
    -- in the /IAM User Guide/.
    path :: Prelude.Maybe Prelude.Text,
    -- | The name of the IAM policy that you have configured in each account
    -- where you want to deploy your permission set.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CustomerManagedPolicyReference' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'path', 'customerManagedPolicyReference_path' - The path to the IAM policy that you have configured in each account
-- where you want to deploy your permission set. The default is @\/@. For
-- more information, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_identifiers.html#identifiers-friendly-names Friendly names and paths>
-- in the /IAM User Guide/.
--
-- 'name', 'customerManagedPolicyReference_name' - The name of the IAM policy that you have configured in each account
-- where you want to deploy your permission set.
newCustomerManagedPolicyReference ::
  -- | 'name'
  Prelude.Text ->
  CustomerManagedPolicyReference
newCustomerManagedPolicyReference pName_ =
  CustomerManagedPolicyReference'
    { path =
        Prelude.Nothing,
      name = pName_
    }

-- | The path to the IAM policy that you have configured in each account
-- where you want to deploy your permission set. The default is @\/@. For
-- more information, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_identifiers.html#identifiers-friendly-names Friendly names and paths>
-- in the /IAM User Guide/.
customerManagedPolicyReference_path :: Lens.Lens' CustomerManagedPolicyReference (Prelude.Maybe Prelude.Text)
customerManagedPolicyReference_path = Lens.lens (\CustomerManagedPolicyReference' {path} -> path) (\s@CustomerManagedPolicyReference' {} a -> s {path = a} :: CustomerManagedPolicyReference)

-- | The name of the IAM policy that you have configured in each account
-- where you want to deploy your permission set.
customerManagedPolicyReference_name :: Lens.Lens' CustomerManagedPolicyReference Prelude.Text
customerManagedPolicyReference_name = Lens.lens (\CustomerManagedPolicyReference' {name} -> name) (\s@CustomerManagedPolicyReference' {} a -> s {name = a} :: CustomerManagedPolicyReference)

instance Data.FromJSON CustomerManagedPolicyReference where
  parseJSON =
    Data.withObject
      "CustomerManagedPolicyReference"
      ( \x ->
          CustomerManagedPolicyReference'
            Prelude.<$> (x Data..:? "Path") Prelude.<*> (x Data..: "Name")
      )

instance
  Prelude.Hashable
    CustomerManagedPolicyReference
  where
  hashWithSalt
    _salt
    CustomerManagedPolicyReference' {..} =
      _salt `Prelude.hashWithSalt` path
        `Prelude.hashWithSalt` name

instance
  Prelude.NFData
    CustomerManagedPolicyReference
  where
  rnf CustomerManagedPolicyReference' {..} =
    Prelude.rnf path `Prelude.seq` Prelude.rnf name

instance Data.ToJSON CustomerManagedPolicyReference where
  toJSON CustomerManagedPolicyReference' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Path" Data..=) Prelude.<$> path,
            Prelude.Just ("Name" Data..= name)
          ]
      )
