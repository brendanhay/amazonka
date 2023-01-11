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
-- Module      : Amazonka.OpenSearchServerless.Types.SecurityPolicyDetail
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpenSearchServerless.Types.SecurityPolicyDetail where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpenSearchServerless.Types.Document
import Amazonka.OpenSearchServerless.Types.SecurityPolicyType
import qualified Amazonka.Prelude as Prelude

-- | Details about an OpenSearch Serverless security policy.
--
-- /See:/ 'newSecurityPolicyDetail' smart constructor.
data SecurityPolicyDetail = SecurityPolicyDetail'
  { -- | The date the policy was created.
    createdDate :: Prelude.Maybe Prelude.Integer,
    -- | The description of the security policy.
    description :: Prelude.Maybe Prelude.Text,
    -- | The timestamp of when the policy was last modified.
    lastModifiedDate :: Prelude.Maybe Prelude.Integer,
    -- | The name of the policy.
    name :: Prelude.Maybe Prelude.Text,
    -- | The JSON policy document without any whitespaces.
    policy :: Prelude.Maybe Document,
    -- | The version of the policy.
    policyVersion :: Prelude.Maybe Prelude.Text,
    -- | The type of security policy.
    type' :: Prelude.Maybe SecurityPolicyType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SecurityPolicyDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdDate', 'securityPolicyDetail_createdDate' - The date the policy was created.
--
-- 'description', 'securityPolicyDetail_description' - The description of the security policy.
--
-- 'lastModifiedDate', 'securityPolicyDetail_lastModifiedDate' - The timestamp of when the policy was last modified.
--
-- 'name', 'securityPolicyDetail_name' - The name of the policy.
--
-- 'policy', 'securityPolicyDetail_policy' - The JSON policy document without any whitespaces.
--
-- 'policyVersion', 'securityPolicyDetail_policyVersion' - The version of the policy.
--
-- 'type'', 'securityPolicyDetail_type' - The type of security policy.
newSecurityPolicyDetail ::
  SecurityPolicyDetail
newSecurityPolicyDetail =
  SecurityPolicyDetail'
    { createdDate =
        Prelude.Nothing,
      description = Prelude.Nothing,
      lastModifiedDate = Prelude.Nothing,
      name = Prelude.Nothing,
      policy = Prelude.Nothing,
      policyVersion = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The date the policy was created.
securityPolicyDetail_createdDate :: Lens.Lens' SecurityPolicyDetail (Prelude.Maybe Prelude.Integer)
securityPolicyDetail_createdDate = Lens.lens (\SecurityPolicyDetail' {createdDate} -> createdDate) (\s@SecurityPolicyDetail' {} a -> s {createdDate = a} :: SecurityPolicyDetail)

-- | The description of the security policy.
securityPolicyDetail_description :: Lens.Lens' SecurityPolicyDetail (Prelude.Maybe Prelude.Text)
securityPolicyDetail_description = Lens.lens (\SecurityPolicyDetail' {description} -> description) (\s@SecurityPolicyDetail' {} a -> s {description = a} :: SecurityPolicyDetail)

-- | The timestamp of when the policy was last modified.
securityPolicyDetail_lastModifiedDate :: Lens.Lens' SecurityPolicyDetail (Prelude.Maybe Prelude.Integer)
securityPolicyDetail_lastModifiedDate = Lens.lens (\SecurityPolicyDetail' {lastModifiedDate} -> lastModifiedDate) (\s@SecurityPolicyDetail' {} a -> s {lastModifiedDate = a} :: SecurityPolicyDetail)

-- | The name of the policy.
securityPolicyDetail_name :: Lens.Lens' SecurityPolicyDetail (Prelude.Maybe Prelude.Text)
securityPolicyDetail_name = Lens.lens (\SecurityPolicyDetail' {name} -> name) (\s@SecurityPolicyDetail' {} a -> s {name = a} :: SecurityPolicyDetail)

-- | The JSON policy document without any whitespaces.
securityPolicyDetail_policy :: Lens.Lens' SecurityPolicyDetail (Prelude.Maybe Document)
securityPolicyDetail_policy = Lens.lens (\SecurityPolicyDetail' {policy} -> policy) (\s@SecurityPolicyDetail' {} a -> s {policy = a} :: SecurityPolicyDetail)

-- | The version of the policy.
securityPolicyDetail_policyVersion :: Lens.Lens' SecurityPolicyDetail (Prelude.Maybe Prelude.Text)
securityPolicyDetail_policyVersion = Lens.lens (\SecurityPolicyDetail' {policyVersion} -> policyVersion) (\s@SecurityPolicyDetail' {} a -> s {policyVersion = a} :: SecurityPolicyDetail)

-- | The type of security policy.
securityPolicyDetail_type :: Lens.Lens' SecurityPolicyDetail (Prelude.Maybe SecurityPolicyType)
securityPolicyDetail_type = Lens.lens (\SecurityPolicyDetail' {type'} -> type') (\s@SecurityPolicyDetail' {} a -> s {type' = a} :: SecurityPolicyDetail)

instance Data.FromJSON SecurityPolicyDetail where
  parseJSON =
    Data.withObject
      "SecurityPolicyDetail"
      ( \x ->
          SecurityPolicyDetail'
            Prelude.<$> (x Data..:? "createdDate")
            Prelude.<*> (x Data..:? "description")
            Prelude.<*> (x Data..:? "lastModifiedDate")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "policy")
            Prelude.<*> (x Data..:? "policyVersion")
            Prelude.<*> (x Data..:? "type")
      )

instance Prelude.Hashable SecurityPolicyDetail where
  hashWithSalt _salt SecurityPolicyDetail' {..} =
    _salt `Prelude.hashWithSalt` createdDate
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` lastModifiedDate
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` policy
      `Prelude.hashWithSalt` policyVersion
      `Prelude.hashWithSalt` type'

instance Prelude.NFData SecurityPolicyDetail where
  rnf SecurityPolicyDetail' {..} =
    Prelude.rnf createdDate
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf lastModifiedDate
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf policy
      `Prelude.seq` Prelude.rnf policyVersion
      `Prelude.seq` Prelude.rnf type'
