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
-- Module      : Amazonka.OpenSearchServerless.Types.AccessPolicySummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpenSearchServerless.Types.AccessPolicySummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpenSearchServerless.Types.AccessPolicyType
import qualified Amazonka.Prelude as Prelude

-- | A summary of the data access policy.
--
-- /See:/ 'newAccessPolicySummary' smart constructor.
data AccessPolicySummary = AccessPolicySummary'
  { -- | The Epoch time when the access policy was created.
    createdDate :: Prelude.Maybe Prelude.Integer,
    -- | The description of the access policy.
    description :: Prelude.Maybe Prelude.Text,
    -- | The date and time when the collection was last modified.
    lastModifiedDate :: Prelude.Maybe Prelude.Integer,
    -- | The name of the access policy.
    name :: Prelude.Maybe Prelude.Text,
    -- | The version of the policy.
    policyVersion :: Prelude.Maybe Prelude.Text,
    -- | The type of access policy. Currently the only available type is @data@.
    type' :: Prelude.Maybe AccessPolicyType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AccessPolicySummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdDate', 'accessPolicySummary_createdDate' - The Epoch time when the access policy was created.
--
-- 'description', 'accessPolicySummary_description' - The description of the access policy.
--
-- 'lastModifiedDate', 'accessPolicySummary_lastModifiedDate' - The date and time when the collection was last modified.
--
-- 'name', 'accessPolicySummary_name' - The name of the access policy.
--
-- 'policyVersion', 'accessPolicySummary_policyVersion' - The version of the policy.
--
-- 'type'', 'accessPolicySummary_type' - The type of access policy. Currently the only available type is @data@.
newAccessPolicySummary ::
  AccessPolicySummary
newAccessPolicySummary =
  AccessPolicySummary'
    { createdDate = Prelude.Nothing,
      description = Prelude.Nothing,
      lastModifiedDate = Prelude.Nothing,
      name = Prelude.Nothing,
      policyVersion = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The Epoch time when the access policy was created.
accessPolicySummary_createdDate :: Lens.Lens' AccessPolicySummary (Prelude.Maybe Prelude.Integer)
accessPolicySummary_createdDate = Lens.lens (\AccessPolicySummary' {createdDate} -> createdDate) (\s@AccessPolicySummary' {} a -> s {createdDate = a} :: AccessPolicySummary)

-- | The description of the access policy.
accessPolicySummary_description :: Lens.Lens' AccessPolicySummary (Prelude.Maybe Prelude.Text)
accessPolicySummary_description = Lens.lens (\AccessPolicySummary' {description} -> description) (\s@AccessPolicySummary' {} a -> s {description = a} :: AccessPolicySummary)

-- | The date and time when the collection was last modified.
accessPolicySummary_lastModifiedDate :: Lens.Lens' AccessPolicySummary (Prelude.Maybe Prelude.Integer)
accessPolicySummary_lastModifiedDate = Lens.lens (\AccessPolicySummary' {lastModifiedDate} -> lastModifiedDate) (\s@AccessPolicySummary' {} a -> s {lastModifiedDate = a} :: AccessPolicySummary)

-- | The name of the access policy.
accessPolicySummary_name :: Lens.Lens' AccessPolicySummary (Prelude.Maybe Prelude.Text)
accessPolicySummary_name = Lens.lens (\AccessPolicySummary' {name} -> name) (\s@AccessPolicySummary' {} a -> s {name = a} :: AccessPolicySummary)

-- | The version of the policy.
accessPolicySummary_policyVersion :: Lens.Lens' AccessPolicySummary (Prelude.Maybe Prelude.Text)
accessPolicySummary_policyVersion = Lens.lens (\AccessPolicySummary' {policyVersion} -> policyVersion) (\s@AccessPolicySummary' {} a -> s {policyVersion = a} :: AccessPolicySummary)

-- | The type of access policy. Currently the only available type is @data@.
accessPolicySummary_type :: Lens.Lens' AccessPolicySummary (Prelude.Maybe AccessPolicyType)
accessPolicySummary_type = Lens.lens (\AccessPolicySummary' {type'} -> type') (\s@AccessPolicySummary' {} a -> s {type' = a} :: AccessPolicySummary)

instance Data.FromJSON AccessPolicySummary where
  parseJSON =
    Data.withObject
      "AccessPolicySummary"
      ( \x ->
          AccessPolicySummary'
            Prelude.<$> (x Data..:? "createdDate")
            Prelude.<*> (x Data..:? "description")
            Prelude.<*> (x Data..:? "lastModifiedDate")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "policyVersion")
            Prelude.<*> (x Data..:? "type")
      )

instance Prelude.Hashable AccessPolicySummary where
  hashWithSalt _salt AccessPolicySummary' {..} =
    _salt `Prelude.hashWithSalt` createdDate
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` lastModifiedDate
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` policyVersion
      `Prelude.hashWithSalt` type'

instance Prelude.NFData AccessPolicySummary where
  rnf AccessPolicySummary' {..} =
    Prelude.rnf createdDate
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf lastModifiedDate
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf policyVersion
      `Prelude.seq` Prelude.rnf type'
