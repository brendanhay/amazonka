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
-- Module      : Network.AWS.ServiceCatalog.Types.RecordTag
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.RecordTag where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about a tag, which is a key-value pair.
--
-- /See:/ 'newRecordTag' smart constructor.
data RecordTag = RecordTag'
  { -- | The key for this tag.
    key :: Prelude.Maybe Prelude.Text,
    -- | The value for this tag.
    value :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.FromJSON RecordTag where
  parseJSON =
    Prelude.withObject
      "RecordTag"
      ( \x ->
          RecordTag'
            Prelude.<$> (x Prelude..:? "Key")
            Prelude.<*> (x Prelude..:? "Value")
      )

instance Prelude.Hashable RecordTag

instance Prelude.NFData RecordTag
