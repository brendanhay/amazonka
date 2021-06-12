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

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types.S3Destination
import qualified Network.AWS.Lens as Lens

-- | Describes the location of the updated firmware.
--
-- /See:/ 'newDestination' smart constructor.
data Destination = Destination'
  { -- | Describes the location in S3 of the updated firmware.
    s3Destination :: Core.Maybe S3Destination
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Destination' {s3Destination = Core.Nothing}

-- | Describes the location in S3 of the updated firmware.
destination_s3Destination :: Lens.Lens' Destination (Core.Maybe S3Destination)
destination_s3Destination = Lens.lens (\Destination' {s3Destination} -> s3Destination) (\s@Destination' {} a -> s {s3Destination = a} :: Destination)

instance Core.FromJSON Destination where
  parseJSON =
    Core.withObject
      "Destination"
      ( \x ->
          Destination' Core.<$> (x Core..:? "s3Destination")
      )

instance Core.Hashable Destination

instance Core.NFData Destination

instance Core.ToJSON Destination where
  toJSON Destination' {..} =
    Core.object
      ( Core.catMaybes
          [("s3Destination" Core..=) Core.<$> s3Destination]
      )
