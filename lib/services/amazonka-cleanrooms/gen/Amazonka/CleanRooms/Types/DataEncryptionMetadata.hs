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
-- Module      : Amazonka.CleanRooms.Types.DataEncryptionMetadata
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CleanRooms.Types.DataEncryptionMetadata where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The settings for client-side encryption for cryptographic computing.
--
-- /See:/ 'newDataEncryptionMetadata' smart constructor.
data DataEncryptionMetadata = DataEncryptionMetadata'
  { -- | Indicates whether encrypted tables can contain cleartext data (true) or
    -- are to cryptographically process every column (false).
    allowCleartext :: Prelude.Bool,
    -- | Indicates whether Fingerprint columns can contain duplicate entries
    -- (true) or are to contain only non-repeated values (false).
    allowDuplicates :: Prelude.Bool,
    -- | Indicates whether Fingerprint columns can be joined on any other
    -- Fingerprint column with a different name (true) or can only be joined on
    -- Fingerprint columns of the same name (false).
    allowJoinsOnColumnsWithDifferentNames :: Prelude.Bool,
    -- | Indicates whether NULL values are to be copied as NULL to encrypted
    -- tables (true) or cryptographically processed (false).
    preserveNulls :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DataEncryptionMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'allowCleartext', 'dataEncryptionMetadata_allowCleartext' - Indicates whether encrypted tables can contain cleartext data (true) or
-- are to cryptographically process every column (false).
--
-- 'allowDuplicates', 'dataEncryptionMetadata_allowDuplicates' - Indicates whether Fingerprint columns can contain duplicate entries
-- (true) or are to contain only non-repeated values (false).
--
-- 'allowJoinsOnColumnsWithDifferentNames', 'dataEncryptionMetadata_allowJoinsOnColumnsWithDifferentNames' - Indicates whether Fingerprint columns can be joined on any other
-- Fingerprint column with a different name (true) or can only be joined on
-- Fingerprint columns of the same name (false).
--
-- 'preserveNulls', 'dataEncryptionMetadata_preserveNulls' - Indicates whether NULL values are to be copied as NULL to encrypted
-- tables (true) or cryptographically processed (false).
newDataEncryptionMetadata ::
  -- | 'allowCleartext'
  Prelude.Bool ->
  -- | 'allowDuplicates'
  Prelude.Bool ->
  -- | 'allowJoinsOnColumnsWithDifferentNames'
  Prelude.Bool ->
  -- | 'preserveNulls'
  Prelude.Bool ->
  DataEncryptionMetadata
newDataEncryptionMetadata
  pAllowCleartext_
  pAllowDuplicates_
  pAllowJoinsOnColumnsWithDifferentNames_
  pPreserveNulls_ =
    DataEncryptionMetadata'
      { allowCleartext =
          pAllowCleartext_,
        allowDuplicates = pAllowDuplicates_,
        allowJoinsOnColumnsWithDifferentNames =
          pAllowJoinsOnColumnsWithDifferentNames_,
        preserveNulls = pPreserveNulls_
      }

-- | Indicates whether encrypted tables can contain cleartext data (true) or
-- are to cryptographically process every column (false).
dataEncryptionMetadata_allowCleartext :: Lens.Lens' DataEncryptionMetadata Prelude.Bool
dataEncryptionMetadata_allowCleartext = Lens.lens (\DataEncryptionMetadata' {allowCleartext} -> allowCleartext) (\s@DataEncryptionMetadata' {} a -> s {allowCleartext = a} :: DataEncryptionMetadata)

-- | Indicates whether Fingerprint columns can contain duplicate entries
-- (true) or are to contain only non-repeated values (false).
dataEncryptionMetadata_allowDuplicates :: Lens.Lens' DataEncryptionMetadata Prelude.Bool
dataEncryptionMetadata_allowDuplicates = Lens.lens (\DataEncryptionMetadata' {allowDuplicates} -> allowDuplicates) (\s@DataEncryptionMetadata' {} a -> s {allowDuplicates = a} :: DataEncryptionMetadata)

-- | Indicates whether Fingerprint columns can be joined on any other
-- Fingerprint column with a different name (true) or can only be joined on
-- Fingerprint columns of the same name (false).
dataEncryptionMetadata_allowJoinsOnColumnsWithDifferentNames :: Lens.Lens' DataEncryptionMetadata Prelude.Bool
dataEncryptionMetadata_allowJoinsOnColumnsWithDifferentNames = Lens.lens (\DataEncryptionMetadata' {allowJoinsOnColumnsWithDifferentNames} -> allowJoinsOnColumnsWithDifferentNames) (\s@DataEncryptionMetadata' {} a -> s {allowJoinsOnColumnsWithDifferentNames = a} :: DataEncryptionMetadata)

-- | Indicates whether NULL values are to be copied as NULL to encrypted
-- tables (true) or cryptographically processed (false).
dataEncryptionMetadata_preserveNulls :: Lens.Lens' DataEncryptionMetadata Prelude.Bool
dataEncryptionMetadata_preserveNulls = Lens.lens (\DataEncryptionMetadata' {preserveNulls} -> preserveNulls) (\s@DataEncryptionMetadata' {} a -> s {preserveNulls = a} :: DataEncryptionMetadata)

instance Data.FromJSON DataEncryptionMetadata where
  parseJSON =
    Data.withObject
      "DataEncryptionMetadata"
      ( \x ->
          DataEncryptionMetadata'
            Prelude.<$> (x Data..: "allowCleartext")
            Prelude.<*> (x Data..: "allowDuplicates")
            Prelude.<*> (x Data..: "allowJoinsOnColumnsWithDifferentNames")
            Prelude.<*> (x Data..: "preserveNulls")
      )

instance Prelude.Hashable DataEncryptionMetadata where
  hashWithSalt _salt DataEncryptionMetadata' {..} =
    _salt
      `Prelude.hashWithSalt` allowCleartext
      `Prelude.hashWithSalt` allowDuplicates
      `Prelude.hashWithSalt` allowJoinsOnColumnsWithDifferentNames
      `Prelude.hashWithSalt` preserveNulls

instance Prelude.NFData DataEncryptionMetadata where
  rnf DataEncryptionMetadata' {..} =
    Prelude.rnf allowCleartext
      `Prelude.seq` Prelude.rnf allowDuplicates
      `Prelude.seq` Prelude.rnf allowJoinsOnColumnsWithDifferentNames
      `Prelude.seq` Prelude.rnf preserveNulls

instance Data.ToJSON DataEncryptionMetadata where
  toJSON DataEncryptionMetadata' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("allowCleartext" Data..= allowCleartext),
            Prelude.Just
              ("allowDuplicates" Data..= allowDuplicates),
            Prelude.Just
              ( "allowJoinsOnColumnsWithDifferentNames"
                  Data..= allowJoinsOnColumnsWithDifferentNames
              ),
            Prelude.Just
              ("preserveNulls" Data..= preserveNulls)
          ]
      )
