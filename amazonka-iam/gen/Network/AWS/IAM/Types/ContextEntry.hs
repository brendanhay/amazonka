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
-- Module      : Network.AWS.IAM.Types.ContextEntry
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.ContextEntry where

import Network.AWS.IAM.Types.ContextKeyTypeEnum
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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
  { -- | The value (or values, if the condition context key supports multiple
    -- values) to provide to the simulation when the key is referenced by a
    -- @Condition@ element in an input policy.
    contextKeyValues :: Prelude.Maybe [Prelude.Text],
    -- | The full name of a condition context key, including the service prefix.
    -- For example, @aws:SourceIp@ or @s3:VersionId@.
    contextKeyName :: Prelude.Maybe Prelude.Text,
    -- | The data type of the value (or values) specified in the
    -- @ContextKeyValues@ parameter.
    contextKeyType :: Prelude.Maybe ContextKeyTypeEnum
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ContextEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'contextKeyValues', 'contextEntry_contextKeyValues' - The value (or values, if the condition context key supports multiple
-- values) to provide to the simulation when the key is referenced by a
-- @Condition@ element in an input policy.
--
-- 'contextKeyName', 'contextEntry_contextKeyName' - The full name of a condition context key, including the service prefix.
-- For example, @aws:SourceIp@ or @s3:VersionId@.
--
-- 'contextKeyType', 'contextEntry_contextKeyType' - The data type of the value (or values) specified in the
-- @ContextKeyValues@ parameter.
newContextEntry ::
  ContextEntry
newContextEntry =
  ContextEntry'
    { contextKeyValues = Prelude.Nothing,
      contextKeyName = Prelude.Nothing,
      contextKeyType = Prelude.Nothing
    }

-- | The value (or values, if the condition context key supports multiple
-- values) to provide to the simulation when the key is referenced by a
-- @Condition@ element in an input policy.
contextEntry_contextKeyValues :: Lens.Lens' ContextEntry (Prelude.Maybe [Prelude.Text])
contextEntry_contextKeyValues = Lens.lens (\ContextEntry' {contextKeyValues} -> contextKeyValues) (\s@ContextEntry' {} a -> s {contextKeyValues = a} :: ContextEntry) Prelude.. Lens.mapping Prelude._Coerce

-- | The full name of a condition context key, including the service prefix.
-- For example, @aws:SourceIp@ or @s3:VersionId@.
contextEntry_contextKeyName :: Lens.Lens' ContextEntry (Prelude.Maybe Prelude.Text)
contextEntry_contextKeyName = Lens.lens (\ContextEntry' {contextKeyName} -> contextKeyName) (\s@ContextEntry' {} a -> s {contextKeyName = a} :: ContextEntry)

-- | The data type of the value (or values) specified in the
-- @ContextKeyValues@ parameter.
contextEntry_contextKeyType :: Lens.Lens' ContextEntry (Prelude.Maybe ContextKeyTypeEnum)
contextEntry_contextKeyType = Lens.lens (\ContextEntry' {contextKeyType} -> contextKeyType) (\s@ContextEntry' {} a -> s {contextKeyType = a} :: ContextEntry)

instance Prelude.Hashable ContextEntry

instance Prelude.NFData ContextEntry

instance Prelude.ToQuery ContextEntry where
  toQuery ContextEntry' {..} =
    Prelude.mconcat
      [ "ContextKeyValues"
          Prelude.=: Prelude.toQuery
            ( Prelude.toQueryList "member"
                Prelude.<$> contextKeyValues
            ),
        "ContextKeyName" Prelude.=: contextKeyName,
        "ContextKeyType" Prelude.=: contextKeyType
      ]
