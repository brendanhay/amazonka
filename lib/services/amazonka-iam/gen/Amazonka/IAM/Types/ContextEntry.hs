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
-- Module      : Amazonka.IAM.Types.ContextEntry
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IAM.Types.ContextEntry where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IAM.Types.ContextKeyTypeEnum
import qualified Amazonka.Prelude as Prelude

-- | Contains information about a condition context key. It includes the name
-- of the key and specifies the value (or values, if the context key
-- supports multiple values) to use in the simulation. This information is
-- used when evaluating the @Condition@ elements of the input policies.
--
-- This data type is used as an input parameter to SimulateCustomPolicy and
-- SimulatePrincipalPolicy.
--
-- /See:/ 'newContextEntry' smart constructor.
data ContextEntry = ContextEntry'
  { -- | The full name of a condition context key, including the service prefix.
    -- For example, @aws:SourceIp@ or @s3:VersionId@.
    contextKeyName :: Prelude.Maybe Prelude.Text,
    -- | The data type of the value (or values) specified in the
    -- @ContextKeyValues@ parameter.
    contextKeyType :: Prelude.Maybe ContextKeyTypeEnum,
    -- | The value (or values, if the condition context key supports multiple
    -- values) to provide to the simulation when the key is referenced by a
    -- @Condition@ element in an input policy.
    contextKeyValues :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ContextEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'contextKeyName', 'contextEntry_contextKeyName' - The full name of a condition context key, including the service prefix.
-- For example, @aws:SourceIp@ or @s3:VersionId@.
--
-- 'contextKeyType', 'contextEntry_contextKeyType' - The data type of the value (or values) specified in the
-- @ContextKeyValues@ parameter.
--
-- 'contextKeyValues', 'contextEntry_contextKeyValues' - The value (or values, if the condition context key supports multiple
-- values) to provide to the simulation when the key is referenced by a
-- @Condition@ element in an input policy.
newContextEntry ::
  ContextEntry
newContextEntry =
  ContextEntry'
    { contextKeyName = Prelude.Nothing,
      contextKeyType = Prelude.Nothing,
      contextKeyValues = Prelude.Nothing
    }

-- | The full name of a condition context key, including the service prefix.
-- For example, @aws:SourceIp@ or @s3:VersionId@.
contextEntry_contextKeyName :: Lens.Lens' ContextEntry (Prelude.Maybe Prelude.Text)
contextEntry_contextKeyName = Lens.lens (\ContextEntry' {contextKeyName} -> contextKeyName) (\s@ContextEntry' {} a -> s {contextKeyName = a} :: ContextEntry)

-- | The data type of the value (or values) specified in the
-- @ContextKeyValues@ parameter.
contextEntry_contextKeyType :: Lens.Lens' ContextEntry (Prelude.Maybe ContextKeyTypeEnum)
contextEntry_contextKeyType = Lens.lens (\ContextEntry' {contextKeyType} -> contextKeyType) (\s@ContextEntry' {} a -> s {contextKeyType = a} :: ContextEntry)

-- | The value (or values, if the condition context key supports multiple
-- values) to provide to the simulation when the key is referenced by a
-- @Condition@ element in an input policy.
contextEntry_contextKeyValues :: Lens.Lens' ContextEntry (Prelude.Maybe [Prelude.Text])
contextEntry_contextKeyValues = Lens.lens (\ContextEntry' {contextKeyValues} -> contextKeyValues) (\s@ContextEntry' {} a -> s {contextKeyValues = a} :: ContextEntry) Prelude.. Lens.mapping Lens.coerced

instance Prelude.Hashable ContextEntry where
  hashWithSalt _salt ContextEntry' {..} =
    _salt
      `Prelude.hashWithSalt` contextKeyName
      `Prelude.hashWithSalt` contextKeyType
      `Prelude.hashWithSalt` contextKeyValues

instance Prelude.NFData ContextEntry where
  rnf ContextEntry' {..} =
    Prelude.rnf contextKeyName
      `Prelude.seq` Prelude.rnf contextKeyType
      `Prelude.seq` Prelude.rnf contextKeyValues

instance Data.ToQuery ContextEntry where
  toQuery ContextEntry' {..} =
    Prelude.mconcat
      [ "ContextKeyName" Data.=: contextKeyName,
        "ContextKeyType" Data.=: contextKeyType,
        "ContextKeyValues"
          Data.=: Data.toQuery
            ( Data.toQueryList "member"
                Prelude.<$> contextKeyValues
            )
      ]
