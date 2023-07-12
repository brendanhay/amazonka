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
-- Module      : Amazonka.OpenSearchServerless.Types.AccessPolicyDetail
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpenSearchServerless.Types.AccessPolicyDetail where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpenSearchServerless.Types.AccessPolicyType
import Amazonka.OpenSearchServerless.Types.Document
import qualified Amazonka.Prelude as Prelude

-- | Details about an OpenSearch Serverless access policy.
--
-- /See:/ 'newAccessPolicyDetail' smart constructor.
data AccessPolicyDetail = AccessPolicyDetail'
  { -- | The date the policy was created.
    createdDate :: Prelude.Maybe Prelude.Integer,
    -- | The description of the policy.
    description :: Prelude.Maybe Prelude.Text,
    -- | The timestamp of when the policy was last modified.
    lastModifiedDate :: Prelude.Maybe Prelude.Integer,
    -- | The name of the policy.
    name :: Prelude.Maybe Prelude.Text,
    -- | The JSON policy document without any whitespaces.
    policy :: Prelude.Maybe Document,
    -- | The version of the policy.
    policyVersion :: Prelude.Maybe Prelude.Text,
    -- | The type of access policy.
    type' :: Prelude.Maybe AccessPolicyType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AccessPolicyDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdDate', 'accessPolicyDetail_createdDate' - The date the policy was created.
--
-- 'description', 'accessPolicyDetail_description' - The description of the policy.
--
-- 'lastModifiedDate', 'accessPolicyDetail_lastModifiedDate' - The timestamp of when the policy was last modified.
--
-- 'name', 'accessPolicyDetail_name' - The name of the policy.
--
-- 'policy', 'accessPolicyDetail_policy' - The JSON policy document without any whitespaces.
--
-- 'policyVersion', 'accessPolicyDetail_policyVersion' - The version of the policy.
--
-- 'type'', 'accessPolicyDetail_type' - The type of access policy.
newAccessPolicyDetail ::
  AccessPolicyDetail
newAccessPolicyDetail =
  AccessPolicyDetail'
    { createdDate = Prelude.Nothing,
      description = Prelude.Nothing,
      lastModifiedDate = Prelude.Nothing,
      name = Prelude.Nothing,
      policy = Prelude.Nothing,
      policyVersion = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The date the policy was created.
accessPolicyDetail_createdDate :: Lens.Lens' AccessPolicyDetail (Prelude.Maybe Prelude.Integer)
accessPolicyDetail_createdDate = Lens.lens (\AccessPolicyDetail' {createdDate} -> createdDate) (\s@AccessPolicyDetail' {} a -> s {createdDate = a} :: AccessPolicyDetail)

-- | The description of the policy.
accessPolicyDetail_description :: Lens.Lens' AccessPolicyDetail (Prelude.Maybe Prelude.Text)
accessPolicyDetail_description = Lens.lens (\AccessPolicyDetail' {description} -> description) (\s@AccessPolicyDetail' {} a -> s {description = a} :: AccessPolicyDetail)

-- | The timestamp of when the policy was last modified.
accessPolicyDetail_lastModifiedDate :: Lens.Lens' AccessPolicyDetail (Prelude.Maybe Prelude.Integer)
accessPolicyDetail_lastModifiedDate = Lens.lens (\AccessPolicyDetail' {lastModifiedDate} -> lastModifiedDate) (\s@AccessPolicyDetail' {} a -> s {lastModifiedDate = a} :: AccessPolicyDetail)

-- | The name of the policy.
accessPolicyDetail_name :: Lens.Lens' AccessPolicyDetail (Prelude.Maybe Prelude.Text)
accessPolicyDetail_name = Lens.lens (\AccessPolicyDetail' {name} -> name) (\s@AccessPolicyDetail' {} a -> s {name = a} :: AccessPolicyDetail)

-- | The JSON policy document without any whitespaces.
accessPolicyDetail_policy :: Lens.Lens' AccessPolicyDetail (Prelude.Maybe Document)
accessPolicyDetail_policy = Lens.lens (\AccessPolicyDetail' {policy} -> policy) (\s@AccessPolicyDetail' {} a -> s {policy = a} :: AccessPolicyDetail)

-- | The version of the policy.
accessPolicyDetail_policyVersion :: Lens.Lens' AccessPolicyDetail (Prelude.Maybe Prelude.Text)
accessPolicyDetail_policyVersion = Lens.lens (\AccessPolicyDetail' {policyVersion} -> policyVersion) (\s@AccessPolicyDetail' {} a -> s {policyVersion = a} :: AccessPolicyDetail)

-- | The type of access policy.
accessPolicyDetail_type :: Lens.Lens' AccessPolicyDetail (Prelude.Maybe AccessPolicyType)
accessPolicyDetail_type = Lens.lens (\AccessPolicyDetail' {type'} -> type') (\s@AccessPolicyDetail' {} a -> s {type' = a} :: AccessPolicyDetail)

instance Data.FromJSON AccessPolicyDetail where
  parseJSON =
    Data.withObject
      "AccessPolicyDetail"
      ( \x ->
          AccessPolicyDetail'
            Prelude.<$> (x Data..:? "createdDate")
            Prelude.<*> (x Data..:? "description")
            Prelude.<*> (x Data..:? "lastModifiedDate")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "policy")
            Prelude.<*> (x Data..:? "policyVersion")
            Prelude.<*> (x Data..:? "type")
      )

instance Prelude.Hashable AccessPolicyDetail where
  hashWithSalt _salt AccessPolicyDetail' {..} =
    _salt
      `Prelude.hashWithSalt` createdDate
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` lastModifiedDate
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` policy
      `Prelude.hashWithSalt` policyVersion
      `Prelude.hashWithSalt` type'

instance Prelude.NFData AccessPolicyDetail where
  rnf AccessPolicyDetail' {..} =
    Prelude.rnf createdDate
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf lastModifiedDate
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf policy
      `Prelude.seq` Prelude.rnf policyVersion
      `Prelude.seq` Prelude.rnf type'
