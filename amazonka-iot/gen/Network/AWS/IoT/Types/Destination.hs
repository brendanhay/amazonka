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
-- Module      : Network.AWS.IoT.Types.Destination
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.Destination where

import Network.AWS.IoT.Types.S3Destination
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes the location of the updated firmware.
--
-- /See:/ 'newDestination' smart constructor.
data Destination = Destination'
  { -- | Describes the location in S3 of the updated firmware.
    s3Destination :: Prelude.Maybe S3Destination
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.FromJSON Destination where
  parseJSON =
    Prelude.withObject
      "Destination"
      ( \x ->
          Destination'
            Prelude.<$> (x Prelude..:? "s3Destination")
      )

instance Prelude.Hashable Destination

instance Prelude.NFData Destination

instance Prelude.ToJSON Destination where
  toJSON Destination' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("s3Destination" Prelude..=)
              Prelude.<$> s3Destination
          ]
      )
