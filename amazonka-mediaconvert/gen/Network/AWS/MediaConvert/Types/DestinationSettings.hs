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
-- Module      : Network.AWS.MediaConvert.Types.DestinationSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.DestinationSettings where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.S3DestinationSettings

-- | Settings associated with the destination. Will vary based on the type of
-- destination
--
-- /See:/ 'newDestinationSettings' smart constructor.
data DestinationSettings = DestinationSettings'
  { -- | Settings associated with S3 destination
    s3Settings :: Core.Maybe S3DestinationSettings
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DestinationSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3Settings', 'destinationSettings_s3Settings' - Settings associated with S3 destination
newDestinationSettings ::
  DestinationSettings
newDestinationSettings =
  DestinationSettings' {s3Settings = Core.Nothing}

-- | Settings associated with S3 destination
destinationSettings_s3Settings :: Lens.Lens' DestinationSettings (Core.Maybe S3DestinationSettings)
destinationSettings_s3Settings = Lens.lens (\DestinationSettings' {s3Settings} -> s3Settings) (\s@DestinationSettings' {} a -> s {s3Settings = a} :: DestinationSettings)

instance Core.FromJSON DestinationSettings where
  parseJSON =
    Core.withObject
      "DestinationSettings"
      ( \x ->
          DestinationSettings'
            Core.<$> (x Core..:? "s3Settings")
      )

instance Core.Hashable DestinationSettings

instance Core.NFData DestinationSettings

instance Core.ToJSON DestinationSettings where
  toJSON DestinationSettings' {..} =
    Core.object
      ( Core.catMaybes
          [("s3Settings" Core..=) Core.<$> s3Settings]
      )
