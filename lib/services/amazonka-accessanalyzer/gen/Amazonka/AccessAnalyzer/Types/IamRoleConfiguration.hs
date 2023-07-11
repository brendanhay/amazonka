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
-- Module      : Amazonka.AccessAnalyzer.Types.IamRoleConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AccessAnalyzer.Types.IamRoleConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The proposed access control configuration for an IAM role. You can
-- propose a configuration for a new IAM role or an existing IAM role that
-- you own by specifying the trust policy. If the configuration is for a
-- new IAM role, you must specify the trust policy. If the configuration is
-- for an existing IAM role that you own and you do not propose the trust
-- policy, the access preview uses the existing trust policy for the role.
-- The proposed trust policy cannot be an empty string. For more
-- information about role trust policy limits, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_iam-quotas.html IAM and STS quotas>.
--
-- /See:/ 'newIamRoleConfiguration' smart constructor.
data IamRoleConfiguration = IamRoleConfiguration'
  { -- | The proposed trust policy for the IAM role.
    trustPolicy :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IamRoleConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'trustPolicy', 'iamRoleConfiguration_trustPolicy' - The proposed trust policy for the IAM role.
newIamRoleConfiguration ::
  IamRoleConfiguration
newIamRoleConfiguration =
  IamRoleConfiguration'
    { trustPolicy =
        Prelude.Nothing
    }

-- | The proposed trust policy for the IAM role.
iamRoleConfiguration_trustPolicy :: Lens.Lens' IamRoleConfiguration (Prelude.Maybe Prelude.Text)
iamRoleConfiguration_trustPolicy = Lens.lens (\IamRoleConfiguration' {trustPolicy} -> trustPolicy) (\s@IamRoleConfiguration' {} a -> s {trustPolicy = a} :: IamRoleConfiguration)

instance Data.FromJSON IamRoleConfiguration where
  parseJSON =
    Data.withObject
      "IamRoleConfiguration"
      ( \x ->
          IamRoleConfiguration'
            Prelude.<$> (x Data..:? "trustPolicy")
      )

instance Prelude.Hashable IamRoleConfiguration where
  hashWithSalt _salt IamRoleConfiguration' {..} =
    _salt `Prelude.hashWithSalt` trustPolicy

instance Prelude.NFData IamRoleConfiguration where
  rnf IamRoleConfiguration' {..} =
    Prelude.rnf trustPolicy

instance Data.ToJSON IamRoleConfiguration where
  toJSON IamRoleConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [("trustPolicy" Data..=) Prelude.<$> trustPolicy]
      )
