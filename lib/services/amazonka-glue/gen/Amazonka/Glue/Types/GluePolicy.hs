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
-- Module      : Amazonka.Glue.Types.GluePolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.GluePolicy where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A structure for returning a resource policy.
--
-- /See:/ 'newGluePolicy' smart constructor.
data GluePolicy = GluePolicy'
  { -- | The date and time at which the policy was created.
    createTime :: Prelude.Maybe Data.POSIX,
    -- | Contains the hash value associated with this policy.
    policyHash :: Prelude.Maybe Prelude.Text,
    -- | Contains the requested policy document, in JSON format.
    policyInJson :: Prelude.Maybe Prelude.Text,
    -- | The date and time at which the policy was last updated.
    updateTime :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GluePolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createTime', 'gluePolicy_createTime' - The date and time at which the policy was created.
--
-- 'policyHash', 'gluePolicy_policyHash' - Contains the hash value associated with this policy.
--
-- 'policyInJson', 'gluePolicy_policyInJson' - Contains the requested policy document, in JSON format.
--
-- 'updateTime', 'gluePolicy_updateTime' - The date and time at which the policy was last updated.
newGluePolicy ::
  GluePolicy
newGluePolicy =
  GluePolicy'
    { createTime = Prelude.Nothing,
      policyHash = Prelude.Nothing,
      policyInJson = Prelude.Nothing,
      updateTime = Prelude.Nothing
    }

-- | The date and time at which the policy was created.
gluePolicy_createTime :: Lens.Lens' GluePolicy (Prelude.Maybe Prelude.UTCTime)
gluePolicy_createTime = Lens.lens (\GluePolicy' {createTime} -> createTime) (\s@GluePolicy' {} a -> s {createTime = a} :: GluePolicy) Prelude.. Lens.mapping Data._Time

-- | Contains the hash value associated with this policy.
gluePolicy_policyHash :: Lens.Lens' GluePolicy (Prelude.Maybe Prelude.Text)
gluePolicy_policyHash = Lens.lens (\GluePolicy' {policyHash} -> policyHash) (\s@GluePolicy' {} a -> s {policyHash = a} :: GluePolicy)

-- | Contains the requested policy document, in JSON format.
gluePolicy_policyInJson :: Lens.Lens' GluePolicy (Prelude.Maybe Prelude.Text)
gluePolicy_policyInJson = Lens.lens (\GluePolicy' {policyInJson} -> policyInJson) (\s@GluePolicy' {} a -> s {policyInJson = a} :: GluePolicy)

-- | The date and time at which the policy was last updated.
gluePolicy_updateTime :: Lens.Lens' GluePolicy (Prelude.Maybe Prelude.UTCTime)
gluePolicy_updateTime = Lens.lens (\GluePolicy' {updateTime} -> updateTime) (\s@GluePolicy' {} a -> s {updateTime = a} :: GluePolicy) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON GluePolicy where
  parseJSON =
    Data.withObject
      "GluePolicy"
      ( \x ->
          GluePolicy'
            Prelude.<$> (x Data..:? "CreateTime")
            Prelude.<*> (x Data..:? "PolicyHash")
            Prelude.<*> (x Data..:? "PolicyInJson")
            Prelude.<*> (x Data..:? "UpdateTime")
      )

instance Prelude.Hashable GluePolicy where
  hashWithSalt _salt GluePolicy' {..} =
    _salt
      `Prelude.hashWithSalt` createTime
      `Prelude.hashWithSalt` policyHash
      `Prelude.hashWithSalt` policyInJson
      `Prelude.hashWithSalt` updateTime

instance Prelude.NFData GluePolicy where
  rnf GluePolicy' {..} =
    Prelude.rnf createTime
      `Prelude.seq` Prelude.rnf policyHash
      `Prelude.seq` Prelude.rnf policyInJson
      `Prelude.seq` Prelude.rnf updateTime
