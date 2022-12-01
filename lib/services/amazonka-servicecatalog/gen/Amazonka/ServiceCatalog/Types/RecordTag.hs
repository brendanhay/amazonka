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
-- Module      : Amazonka.ServiceCatalog.Types.RecordTag
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ServiceCatalog.Types.RecordTag where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Information about a tag, which is a key-value pair.
--
-- /See:/ 'newRecordTag' smart constructor.
data RecordTag = RecordTag'
  { -- | The key for this tag.
    key :: Prelude.Maybe Prelude.Text,
    -- | The value for this tag.
    value :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RecordTag' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'key', 'recordTag_key' - The key for this tag.
--
-- 'value', 'recordTag_value' - The value for this tag.
newRecordTag ::
  RecordTag
newRecordTag =
  RecordTag'
    { key = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | The key for this tag.
recordTag_key :: Lens.Lens' RecordTag (Prelude.Maybe Prelude.Text)
recordTag_key = Lens.lens (\RecordTag' {key} -> key) (\s@RecordTag' {} a -> s {key = a} :: RecordTag)

-- | The value for this tag.
recordTag_value :: Lens.Lens' RecordTag (Prelude.Maybe Prelude.Text)
recordTag_value = Lens.lens (\RecordTag' {value} -> value) (\s@RecordTag' {} a -> s {value = a} :: RecordTag)

instance Core.FromJSON RecordTag where
  parseJSON =
    Core.withObject
      "RecordTag"
      ( \x ->
          RecordTag'
            Prelude.<$> (x Core..:? "Key") Prelude.<*> (x Core..:? "Value")
      )

instance Prelude.Hashable RecordTag where
  hashWithSalt _salt RecordTag' {..} =
    _salt `Prelude.hashWithSalt` key
      `Prelude.hashWithSalt` value

instance Prelude.NFData RecordTag where
  rnf RecordTag' {..} =
    Prelude.rnf key `Prelude.seq` Prelude.rnf value
