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
-- Module      : Amazonka.IoT.Types.StartSigningJobParameter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.StartSigningJobParameter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types.Destination
import Amazonka.IoT.Types.SigningProfileParameter
import qualified Amazonka.Prelude as Prelude

-- | Information required to start a signing job.
--
-- /See:/ 'newStartSigningJobParameter' smart constructor.
data StartSigningJobParameter = StartSigningJobParameter'
  { -- | The location to write the code-signed file.
    destination :: Prelude.Maybe Destination,
    -- | The code-signing profile name.
    signingProfileName :: Prelude.Maybe Prelude.Text,
    -- | Describes the code-signing profile.
    signingProfileParameter :: Prelude.Maybe SigningProfileParameter
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartSigningJobParameter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'destination', 'startSigningJobParameter_destination' - The location to write the code-signed file.
--
-- 'signingProfileName', 'startSigningJobParameter_signingProfileName' - The code-signing profile name.
--
-- 'signingProfileParameter', 'startSigningJobParameter_signingProfileParameter' - Describes the code-signing profile.
newStartSigningJobParameter ::
  StartSigningJobParameter
newStartSigningJobParameter =
  StartSigningJobParameter'
    { destination =
        Prelude.Nothing,
      signingProfileName = Prelude.Nothing,
      signingProfileParameter = Prelude.Nothing
    }

-- | The location to write the code-signed file.
startSigningJobParameter_destination :: Lens.Lens' StartSigningJobParameter (Prelude.Maybe Destination)
startSigningJobParameter_destination = Lens.lens (\StartSigningJobParameter' {destination} -> destination) (\s@StartSigningJobParameter' {} a -> s {destination = a} :: StartSigningJobParameter)

-- | The code-signing profile name.
startSigningJobParameter_signingProfileName :: Lens.Lens' StartSigningJobParameter (Prelude.Maybe Prelude.Text)
startSigningJobParameter_signingProfileName = Lens.lens (\StartSigningJobParameter' {signingProfileName} -> signingProfileName) (\s@StartSigningJobParameter' {} a -> s {signingProfileName = a} :: StartSigningJobParameter)

-- | Describes the code-signing profile.
startSigningJobParameter_signingProfileParameter :: Lens.Lens' StartSigningJobParameter (Prelude.Maybe SigningProfileParameter)
startSigningJobParameter_signingProfileParameter = Lens.lens (\StartSigningJobParameter' {signingProfileParameter} -> signingProfileParameter) (\s@StartSigningJobParameter' {} a -> s {signingProfileParameter = a} :: StartSigningJobParameter)

instance Data.FromJSON StartSigningJobParameter where
  parseJSON =
    Data.withObject
      "StartSigningJobParameter"
      ( \x ->
          StartSigningJobParameter'
            Prelude.<$> (x Data..:? "destination")
            Prelude.<*> (x Data..:? "signingProfileName")
            Prelude.<*> (x Data..:? "signingProfileParameter")
      )

instance Prelude.Hashable StartSigningJobParameter where
  hashWithSalt _salt StartSigningJobParameter' {..} =
    _salt
      `Prelude.hashWithSalt` destination
      `Prelude.hashWithSalt` signingProfileName
      `Prelude.hashWithSalt` signingProfileParameter

instance Prelude.NFData StartSigningJobParameter where
  rnf StartSigningJobParameter' {..} =
    Prelude.rnf destination
      `Prelude.seq` Prelude.rnf signingProfileName
      `Prelude.seq` Prelude.rnf signingProfileParameter

instance Data.ToJSON StartSigningJobParameter where
  toJSON StartSigningJobParameter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("destination" Data..=) Prelude.<$> destination,
            ("signingProfileName" Data..=)
              Prelude.<$> signingProfileName,
            ("signingProfileParameter" Data..=)
              Prelude.<$> signingProfileParameter
          ]
      )
