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
-- Module      : Amazonka.Proton.Types.SyncBlockerContext
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Proton.Types.SyncBlockerContext where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Detailed data of the context of the sync blocker.
--
-- /See:/ 'newSyncBlockerContext' smart constructor.
data SyncBlockerContext = SyncBlockerContext'
  { -- | The key for the sync blocker context.
    key :: Prelude.Text,
    -- | The value of the sync blocker context.
    value :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SyncBlockerContext' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'key', 'syncBlockerContext_key' - The key for the sync blocker context.
--
-- 'value', 'syncBlockerContext_value' - The value of the sync blocker context.
newSyncBlockerContext ::
  -- | 'key'
  Prelude.Text ->
  -- | 'value'
  Prelude.Text ->
  SyncBlockerContext
newSyncBlockerContext pKey_ pValue_ =
  SyncBlockerContext' {key = pKey_, value = pValue_}

-- | The key for the sync blocker context.
syncBlockerContext_key :: Lens.Lens' SyncBlockerContext Prelude.Text
syncBlockerContext_key = Lens.lens (\SyncBlockerContext' {key} -> key) (\s@SyncBlockerContext' {} a -> s {key = a} :: SyncBlockerContext)

-- | The value of the sync blocker context.
syncBlockerContext_value :: Lens.Lens' SyncBlockerContext Prelude.Text
syncBlockerContext_value = Lens.lens (\SyncBlockerContext' {value} -> value) (\s@SyncBlockerContext' {} a -> s {value = a} :: SyncBlockerContext)

instance Data.FromJSON SyncBlockerContext where
  parseJSON =
    Data.withObject
      "SyncBlockerContext"
      ( \x ->
          SyncBlockerContext'
            Prelude.<$> (x Data..: "key")
            Prelude.<*> (x Data..: "value")
      )

instance Prelude.Hashable SyncBlockerContext where
  hashWithSalt _salt SyncBlockerContext' {..} =
    _salt
      `Prelude.hashWithSalt` key
      `Prelude.hashWithSalt` value

instance Prelude.NFData SyncBlockerContext where
  rnf SyncBlockerContext' {..} =
    Prelude.rnf key `Prelude.seq` Prelude.rnf value
