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
-- Module      : Amazonka.OpenSearchServerless.Types.SecurityPolicySummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpenSearchServerless.Types.SecurityPolicySummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpenSearchServerless.Types.SecurityPolicyType
import qualified Amazonka.Prelude as Prelude

-- | A summary of a security policy for OpenSearch Serverless.
--
-- /See:/ 'newSecurityPolicySummary' smart constructor.
data SecurityPolicySummary = SecurityPolicySummary'
  { -- | The date the policy was created.
    createdDate :: Prelude.Maybe Prelude.Integer,
    -- | The description of the security policy.
    description :: Prelude.Maybe Prelude.Text,
    -- | The timestamp of when the policy was last modified.
    lastModifiedDate :: Prelude.Maybe Prelude.Integer,
    -- | The name of the policy.
    name :: Prelude.Maybe Prelude.Text,
    -- | The version of the policy.
    policyVersion :: Prelude.Maybe Prelude.Text,
    -- | The type of security policy.
    type' :: Prelude.Maybe SecurityPolicyType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SecurityPolicySummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdDate', 'securityPolicySummary_createdDate' - The date the policy was created.
--
-- 'description', 'securityPolicySummary_description' - The description of the security policy.
--
-- 'lastModifiedDate', 'securityPolicySummary_lastModifiedDate' - The timestamp of when the policy was last modified.
--
-- 'name', 'securityPolicySummary_name' - The name of the policy.
--
-- 'policyVersion', 'securityPolicySummary_policyVersion' - The version of the policy.
--
-- 'type'', 'securityPolicySummary_type' - The type of security policy.
newSecurityPolicySummary ::
  SecurityPolicySummary
newSecurityPolicySummary =
  SecurityPolicySummary'
    { createdDate =
        Prelude.Nothing,
      description = Prelude.Nothing,
      lastModifiedDate = Prelude.Nothing,
      name = Prelude.Nothing,
      policyVersion = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The date the policy was created.
securityPolicySummary_createdDate :: Lens.Lens' SecurityPolicySummary (Prelude.Maybe Prelude.Integer)
securityPolicySummary_createdDate = Lens.lens (\SecurityPolicySummary' {createdDate} -> createdDate) (\s@SecurityPolicySummary' {} a -> s {createdDate = a} :: SecurityPolicySummary)

-- | The description of the security policy.
securityPolicySummary_description :: Lens.Lens' SecurityPolicySummary (Prelude.Maybe Prelude.Text)
securityPolicySummary_description = Lens.lens (\SecurityPolicySummary' {description} -> description) (\s@SecurityPolicySummary' {} a -> s {description = a} :: SecurityPolicySummary)

-- | The timestamp of when the policy was last modified.
securityPolicySummary_lastModifiedDate :: Lens.Lens' SecurityPolicySummary (Prelude.Maybe Prelude.Integer)
securityPolicySummary_lastModifiedDate = Lens.lens (\SecurityPolicySummary' {lastModifiedDate} -> lastModifiedDate) (\s@SecurityPolicySummary' {} a -> s {lastModifiedDate = a} :: SecurityPolicySummary)

-- | The name of the policy.
securityPolicySummary_name :: Lens.Lens' SecurityPolicySummary (Prelude.Maybe Prelude.Text)
securityPolicySummary_name = Lens.lens (\SecurityPolicySummary' {name} -> name) (\s@SecurityPolicySummary' {} a -> s {name = a} :: SecurityPolicySummary)

-- | The version of the policy.
securityPolicySummary_policyVersion :: Lens.Lens' SecurityPolicySummary (Prelude.Maybe Prelude.Text)
securityPolicySummary_policyVersion = Lens.lens (\SecurityPolicySummary' {policyVersion} -> policyVersion) (\s@SecurityPolicySummary' {} a -> s {policyVersion = a} :: SecurityPolicySummary)

-- | The type of security policy.
securityPolicySummary_type :: Lens.Lens' SecurityPolicySummary (Prelude.Maybe SecurityPolicyType)
securityPolicySummary_type = Lens.lens (\SecurityPolicySummary' {type'} -> type') (\s@SecurityPolicySummary' {} a -> s {type' = a} :: SecurityPolicySummary)

instance Data.FromJSON SecurityPolicySummary where
  parseJSON =
    Data.withObject
      "SecurityPolicySummary"
      ( \x ->
          SecurityPolicySummary'
            Prelude.<$> (x Data..:? "createdDate")
            Prelude.<*> (x Data..:? "description")
            Prelude.<*> (x Data..:? "lastModifiedDate")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "policyVersion")
            Prelude.<*> (x Data..:? "type")
      )

instance Prelude.Hashable SecurityPolicySummary where
  hashWithSalt _salt SecurityPolicySummary' {..} =
    _salt `Prelude.hashWithSalt` createdDate
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` lastModifiedDate
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` policyVersion
      `Prelude.hashWithSalt` type'

instance Prelude.NFData SecurityPolicySummary where
  rnf SecurityPolicySummary' {..} =
    Prelude.rnf createdDate
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf lastModifiedDate
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf policyVersion
      `Prelude.seq` Prelude.rnf type'
