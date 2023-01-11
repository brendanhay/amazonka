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
-- Module      : Amazonka.MediaConvert.Types.FileGroupSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.FileGroupSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConvert.Types.DestinationSettings
import qualified Amazonka.Prelude as Prelude

-- | Settings related to your File output group. MediaConvert uses this group
-- of settings to generate a single standalone file, rather than a
-- streaming package. When you work directly in your JSON job
-- specification, include this object and any required children when you
-- set Type, under OutputGroupSettings, to FILE_GROUP_SETTINGS.
--
-- /See:/ 'newFileGroupSettings' smart constructor.
data FileGroupSettings = FileGroupSettings'
  { -- | Use Destination (Destination) to specify the S3 output location and the
    -- output filename base. Destination accepts format identifiers. If you do
    -- not specify the base filename in the URI, the service will use the
    -- filename of the input file. If your job has multiple inputs, the service
    -- uses the filename of the first input file.
    destination :: Prelude.Maybe Prelude.Text,
    -- | Settings associated with the destination. Will vary based on the type of
    -- destination
    destinationSettings :: Prelude.Maybe DestinationSettings
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FileGroupSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'destination', 'fileGroupSettings_destination' - Use Destination (Destination) to specify the S3 output location and the
-- output filename base. Destination accepts format identifiers. If you do
-- not specify the base filename in the URI, the service will use the
-- filename of the input file. If your job has multiple inputs, the service
-- uses the filename of the first input file.
--
-- 'destinationSettings', 'fileGroupSettings_destinationSettings' - Settings associated with the destination. Will vary based on the type of
-- destination
newFileGroupSettings ::
  FileGroupSettings
newFileGroupSettings =
  FileGroupSettings'
    { destination = Prelude.Nothing,
      destinationSettings = Prelude.Nothing
    }

-- | Use Destination (Destination) to specify the S3 output location and the
-- output filename base. Destination accepts format identifiers. If you do
-- not specify the base filename in the URI, the service will use the
-- filename of the input file. If your job has multiple inputs, the service
-- uses the filename of the first input file.
fileGroupSettings_destination :: Lens.Lens' FileGroupSettings (Prelude.Maybe Prelude.Text)
fileGroupSettings_destination = Lens.lens (\FileGroupSettings' {destination} -> destination) (\s@FileGroupSettings' {} a -> s {destination = a} :: FileGroupSettings)

-- | Settings associated with the destination. Will vary based on the type of
-- destination
fileGroupSettings_destinationSettings :: Lens.Lens' FileGroupSettings (Prelude.Maybe DestinationSettings)
fileGroupSettings_destinationSettings = Lens.lens (\FileGroupSettings' {destinationSettings} -> destinationSettings) (\s@FileGroupSettings' {} a -> s {destinationSettings = a} :: FileGroupSettings)

instance Data.FromJSON FileGroupSettings where
  parseJSON =
    Data.withObject
      "FileGroupSettings"
      ( \x ->
          FileGroupSettings'
            Prelude.<$> (x Data..:? "destination")
            Prelude.<*> (x Data..:? "destinationSettings")
      )

instance Prelude.Hashable FileGroupSettings where
  hashWithSalt _salt FileGroupSettings' {..} =
    _salt `Prelude.hashWithSalt` destination
      `Prelude.hashWithSalt` destinationSettings

instance Prelude.NFData FileGroupSettings where
  rnf FileGroupSettings' {..} =
    Prelude.rnf destination
      `Prelude.seq` Prelude.rnf destinationSettings

instance Data.ToJSON FileGroupSettings where
  toJSON FileGroupSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("destination" Data..=) Prelude.<$> destination,
            ("destinationSettings" Data..=)
              Prelude.<$> destinationSettings
          ]
      )
