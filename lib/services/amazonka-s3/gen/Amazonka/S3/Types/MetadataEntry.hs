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
-- Module      : Amazonka.S3.Types.MetadataEntry
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.S3.Types.MetadataEntry where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.S3.Internal

-- | A metadata key-value pair to store with an object.
--
-- /See:/ 'newMetadataEntry' smart constructor.
data MetadataEntry = MetadataEntry'
  { -- | Name of the Object.
    name :: Prelude.Maybe Prelude.Text,
    -- | Value of the Object.
    value :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MetadataEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'metadataEntry_name' - Name of the Object.
--
-- 'value', 'metadataEntry_value' - Value of the Object.
newMetadataEntry ::
  MetadataEntry
newMetadataEntry =
  MetadataEntry'
    { name = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | Name of the Object.
metadataEntry_name :: Lens.Lens' MetadataEntry (Prelude.Maybe Prelude.Text)
metadataEntry_name = Lens.lens (\MetadataEntry' {name} -> name) (\s@MetadataEntry' {} a -> s {name = a} :: MetadataEntry)

-- | Value of the Object.
metadataEntry_value :: Lens.Lens' MetadataEntry (Prelude.Maybe Prelude.Text)
metadataEntry_value = Lens.lens (\MetadataEntry' {value} -> value) (\s@MetadataEntry' {} a -> s {value = a} :: MetadataEntry)

instance Prelude.Hashable MetadataEntry where
  hashWithSalt _salt MetadataEntry' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` value

instance Prelude.NFData MetadataEntry where
  rnf MetadataEntry' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf value

instance Data.ToXML MetadataEntry where
  toXML MetadataEntry' {..} =
    Prelude.mconcat
      ["Name" Data.@= name, "Value" Data.@= value]
