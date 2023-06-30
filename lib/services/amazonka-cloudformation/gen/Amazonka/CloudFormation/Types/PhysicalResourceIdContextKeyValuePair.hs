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
-- Module      : Amazonka.CloudFormation.Types.PhysicalResourceIdContextKeyValuePair
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFormation.Types.PhysicalResourceIdContextKeyValuePair where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Context information that enables CloudFormation to uniquely identify a
-- resource. CloudFormation uses context key-value pairs in cases where a
-- resource\'s logical and physical IDs aren\'t enough to uniquely identify
-- that resource. Each context key-value pair specifies a resource that
-- contains the targeted resource.
--
-- /See:/ 'newPhysicalResourceIdContextKeyValuePair' smart constructor.
data PhysicalResourceIdContextKeyValuePair = PhysicalResourceIdContextKeyValuePair'
  { -- | The resource context key.
    key :: Prelude.Text,
    -- | The resource context value.
    value :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PhysicalResourceIdContextKeyValuePair' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'key', 'physicalResourceIdContextKeyValuePair_key' - The resource context key.
--
-- 'value', 'physicalResourceIdContextKeyValuePair_value' - The resource context value.
newPhysicalResourceIdContextKeyValuePair ::
  -- | 'key'
  Prelude.Text ->
  -- | 'value'
  Prelude.Text ->
  PhysicalResourceIdContextKeyValuePair
newPhysicalResourceIdContextKeyValuePair
  pKey_
  pValue_ =
    PhysicalResourceIdContextKeyValuePair'
      { key = pKey_,
        value = pValue_
      }

-- | The resource context key.
physicalResourceIdContextKeyValuePair_key :: Lens.Lens' PhysicalResourceIdContextKeyValuePair Prelude.Text
physicalResourceIdContextKeyValuePair_key = Lens.lens (\PhysicalResourceIdContextKeyValuePair' {key} -> key) (\s@PhysicalResourceIdContextKeyValuePair' {} a -> s {key = a} :: PhysicalResourceIdContextKeyValuePair)

-- | The resource context value.
physicalResourceIdContextKeyValuePair_value :: Lens.Lens' PhysicalResourceIdContextKeyValuePair Prelude.Text
physicalResourceIdContextKeyValuePair_value = Lens.lens (\PhysicalResourceIdContextKeyValuePair' {value} -> value) (\s@PhysicalResourceIdContextKeyValuePair' {} a -> s {value = a} :: PhysicalResourceIdContextKeyValuePair)

instance
  Data.FromXML
    PhysicalResourceIdContextKeyValuePair
  where
  parseXML x =
    PhysicalResourceIdContextKeyValuePair'
      Prelude.<$> (x Data..@ "Key")
      Prelude.<*> (x Data..@ "Value")

instance
  Prelude.Hashable
    PhysicalResourceIdContextKeyValuePair
  where
  hashWithSalt
    _salt
    PhysicalResourceIdContextKeyValuePair' {..} =
      _salt
        `Prelude.hashWithSalt` key
        `Prelude.hashWithSalt` value

instance
  Prelude.NFData
    PhysicalResourceIdContextKeyValuePair
  where
  rnf PhysicalResourceIdContextKeyValuePair' {..} =
    Prelude.rnf key `Prelude.seq` Prelude.rnf value
