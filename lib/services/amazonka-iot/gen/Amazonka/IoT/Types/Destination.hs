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
-- Module      : Amazonka.IoT.Types.Destination
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.Destination where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoT.Types.S3Destination
import qualified Amazonka.Prelude as Prelude

-- | Describes the location of the updated firmware.
--
-- /See:/ 'newDestination' smart constructor.
data Destination = Destination'
  { -- | Describes the location in S3 of the updated firmware.
    s3Destination :: Prelude.Maybe S3Destination
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Destination' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3Destination', 'destination_s3Destination' - Describes the location in S3 of the updated firmware.
newDestination ::
  Destination
newDestination =
  Destination' {s3Destination = Prelude.Nothing}

-- | Describes the location in S3 of the updated firmware.
destination_s3Destination :: Lens.Lens' Destination (Prelude.Maybe S3Destination)
destination_s3Destination = Lens.lens (\Destination' {s3Destination} -> s3Destination) (\s@Destination' {} a -> s {s3Destination = a} :: Destination)

instance Core.FromJSON Destination where
  parseJSON =
    Core.withObject
      "Destination"
      ( \x ->
          Destination'
            Prelude.<$> (x Core..:? "s3Destination")
      )

instance Prelude.Hashable Destination where
  hashWithSalt _salt Destination' {..} =
    _salt `Prelude.hashWithSalt` s3Destination

instance Prelude.NFData Destination where
  rnf Destination' {..} = Prelude.rnf s3Destination

instance Core.ToJSON Destination where
  toJSON Destination' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("s3Destination" Core..=)
              Prelude.<$> s3Destination
          ]
      )
