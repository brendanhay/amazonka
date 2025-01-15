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
-- Module      : Amazonka.Transfer.Types.DecryptStepDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Transfer.Types.DecryptStepDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Transfer.Types.EncryptionType
import Amazonka.Transfer.Types.InputFileLocation
import Amazonka.Transfer.Types.OverwriteExisting

-- | /See:/ 'newDecryptStepDetails' smart constructor.
data DecryptStepDetails = DecryptStepDetails'
  { name :: Prelude.Maybe Prelude.Text,
    overwriteExisting :: Prelude.Maybe OverwriteExisting,
    sourceFileLocation :: Prelude.Maybe Prelude.Text,
    type' :: EncryptionType,
    destinationFileLocation :: InputFileLocation
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DecryptStepDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'decryptStepDetails_name' - Undocumented member.
--
-- 'overwriteExisting', 'decryptStepDetails_overwriteExisting' - Undocumented member.
--
-- 'sourceFileLocation', 'decryptStepDetails_sourceFileLocation' - Undocumented member.
--
-- 'type'', 'decryptStepDetails_type' - Undocumented member.
--
-- 'destinationFileLocation', 'decryptStepDetails_destinationFileLocation' - Undocumented member.
newDecryptStepDetails ::
  -- | 'type''
  EncryptionType ->
  -- | 'destinationFileLocation'
  InputFileLocation ->
  DecryptStepDetails
newDecryptStepDetails
  pType_
  pDestinationFileLocation_ =
    DecryptStepDetails'
      { name = Prelude.Nothing,
        overwriteExisting = Prelude.Nothing,
        sourceFileLocation = Prelude.Nothing,
        type' = pType_,
        destinationFileLocation = pDestinationFileLocation_
      }

-- | Undocumented member.
decryptStepDetails_name :: Lens.Lens' DecryptStepDetails (Prelude.Maybe Prelude.Text)
decryptStepDetails_name = Lens.lens (\DecryptStepDetails' {name} -> name) (\s@DecryptStepDetails' {} a -> s {name = a} :: DecryptStepDetails)

-- | Undocumented member.
decryptStepDetails_overwriteExisting :: Lens.Lens' DecryptStepDetails (Prelude.Maybe OverwriteExisting)
decryptStepDetails_overwriteExisting = Lens.lens (\DecryptStepDetails' {overwriteExisting} -> overwriteExisting) (\s@DecryptStepDetails' {} a -> s {overwriteExisting = a} :: DecryptStepDetails)

-- | Undocumented member.
decryptStepDetails_sourceFileLocation :: Lens.Lens' DecryptStepDetails (Prelude.Maybe Prelude.Text)
decryptStepDetails_sourceFileLocation = Lens.lens (\DecryptStepDetails' {sourceFileLocation} -> sourceFileLocation) (\s@DecryptStepDetails' {} a -> s {sourceFileLocation = a} :: DecryptStepDetails)

-- | Undocumented member.
decryptStepDetails_type :: Lens.Lens' DecryptStepDetails EncryptionType
decryptStepDetails_type = Lens.lens (\DecryptStepDetails' {type'} -> type') (\s@DecryptStepDetails' {} a -> s {type' = a} :: DecryptStepDetails)

-- | Undocumented member.
decryptStepDetails_destinationFileLocation :: Lens.Lens' DecryptStepDetails InputFileLocation
decryptStepDetails_destinationFileLocation = Lens.lens (\DecryptStepDetails' {destinationFileLocation} -> destinationFileLocation) (\s@DecryptStepDetails' {} a -> s {destinationFileLocation = a} :: DecryptStepDetails)

instance Data.FromJSON DecryptStepDetails where
  parseJSON =
    Data.withObject
      "DecryptStepDetails"
      ( \x ->
          DecryptStepDetails'
            Prelude.<$> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "OverwriteExisting")
            Prelude.<*> (x Data..:? "SourceFileLocation")
            Prelude.<*> (x Data..: "Type")
            Prelude.<*> (x Data..: "DestinationFileLocation")
      )

instance Prelude.Hashable DecryptStepDetails where
  hashWithSalt _salt DecryptStepDetails' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` overwriteExisting
      `Prelude.hashWithSalt` sourceFileLocation
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` destinationFileLocation

instance Prelude.NFData DecryptStepDetails where
  rnf DecryptStepDetails' {..} =
    Prelude.rnf name `Prelude.seq`
      Prelude.rnf overwriteExisting `Prelude.seq`
        Prelude.rnf sourceFileLocation `Prelude.seq`
          Prelude.rnf type' `Prelude.seq`
            Prelude.rnf destinationFileLocation

instance Data.ToJSON DecryptStepDetails where
  toJSON DecryptStepDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Name" Data..=) Prelude.<$> name,
            ("OverwriteExisting" Data..=)
              Prelude.<$> overwriteExisting,
            ("SourceFileLocation" Data..=)
              Prelude.<$> sourceFileLocation,
            Prelude.Just ("Type" Data..= type'),
            Prelude.Just
              ( "DestinationFileLocation"
                  Data..= destinationFileLocation
              )
          ]
      )
