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
-- Module      : Network.AWS.DirectoryService.Types.DirectoryLimits
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectoryService.Types.DirectoryLimits where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Contains directory limit information for a Region.
--
-- /See:/ 'newDirectoryLimits' smart constructor.
data DirectoryLimits = DirectoryLimits'
  { -- | The maximum number of cloud directories allowed in the Region.
    cloudOnlyDirectoriesLimit :: Core.Maybe Core.Natural,
    -- | Indicates if the connected directory limit has been reached.
    connectedDirectoriesLimitReached :: Core.Maybe Core.Bool,
    -- | The current number of AWS Managed Microsoft AD directories in the
    -- region.
    cloudOnlyMicrosoftADCurrentCount :: Core.Maybe Core.Natural,
    -- | The maximum number of connected directories allowed in the Region.
    connectedDirectoriesLimit :: Core.Maybe Core.Natural,
    -- | The current number of connected directories in the Region.
    connectedDirectoriesCurrentCount :: Core.Maybe Core.Natural,
    -- | The maximum number of AWS Managed Microsoft AD directories allowed in
    -- the region.
    cloudOnlyMicrosoftADLimit :: Core.Maybe Core.Natural,
    -- | Indicates if the cloud directory limit has been reached.
    cloudOnlyDirectoriesLimitReached :: Core.Maybe Core.Bool,
    -- | The current number of cloud directories in the Region.
    cloudOnlyDirectoriesCurrentCount :: Core.Maybe Core.Natural,
    -- | Indicates if the AWS Managed Microsoft AD directory limit has been
    -- reached.
    cloudOnlyMicrosoftADLimitReached :: Core.Maybe Core.Bool
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DirectoryLimits' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cloudOnlyDirectoriesLimit', 'directoryLimits_cloudOnlyDirectoriesLimit' - The maximum number of cloud directories allowed in the Region.
--
-- 'connectedDirectoriesLimitReached', 'directoryLimits_connectedDirectoriesLimitReached' - Indicates if the connected directory limit has been reached.
--
-- 'cloudOnlyMicrosoftADCurrentCount', 'directoryLimits_cloudOnlyMicrosoftADCurrentCount' - The current number of AWS Managed Microsoft AD directories in the
-- region.
--
-- 'connectedDirectoriesLimit', 'directoryLimits_connectedDirectoriesLimit' - The maximum number of connected directories allowed in the Region.
--
-- 'connectedDirectoriesCurrentCount', 'directoryLimits_connectedDirectoriesCurrentCount' - The current number of connected directories in the Region.
--
-- 'cloudOnlyMicrosoftADLimit', 'directoryLimits_cloudOnlyMicrosoftADLimit' - The maximum number of AWS Managed Microsoft AD directories allowed in
-- the region.
--
-- 'cloudOnlyDirectoriesLimitReached', 'directoryLimits_cloudOnlyDirectoriesLimitReached' - Indicates if the cloud directory limit has been reached.
--
-- 'cloudOnlyDirectoriesCurrentCount', 'directoryLimits_cloudOnlyDirectoriesCurrentCount' - The current number of cloud directories in the Region.
--
-- 'cloudOnlyMicrosoftADLimitReached', 'directoryLimits_cloudOnlyMicrosoftADLimitReached' - Indicates if the AWS Managed Microsoft AD directory limit has been
-- reached.
newDirectoryLimits ::
  DirectoryLimits
newDirectoryLimits =
  DirectoryLimits'
    { cloudOnlyDirectoriesLimit =
        Core.Nothing,
      connectedDirectoriesLimitReached = Core.Nothing,
      cloudOnlyMicrosoftADCurrentCount = Core.Nothing,
      connectedDirectoriesLimit = Core.Nothing,
      connectedDirectoriesCurrentCount = Core.Nothing,
      cloudOnlyMicrosoftADLimit = Core.Nothing,
      cloudOnlyDirectoriesLimitReached = Core.Nothing,
      cloudOnlyDirectoriesCurrentCount = Core.Nothing,
      cloudOnlyMicrosoftADLimitReached = Core.Nothing
    }

-- | The maximum number of cloud directories allowed in the Region.
directoryLimits_cloudOnlyDirectoriesLimit :: Lens.Lens' DirectoryLimits (Core.Maybe Core.Natural)
directoryLimits_cloudOnlyDirectoriesLimit = Lens.lens (\DirectoryLimits' {cloudOnlyDirectoriesLimit} -> cloudOnlyDirectoriesLimit) (\s@DirectoryLimits' {} a -> s {cloudOnlyDirectoriesLimit = a} :: DirectoryLimits)

-- | Indicates if the connected directory limit has been reached.
directoryLimits_connectedDirectoriesLimitReached :: Lens.Lens' DirectoryLimits (Core.Maybe Core.Bool)
directoryLimits_connectedDirectoriesLimitReached = Lens.lens (\DirectoryLimits' {connectedDirectoriesLimitReached} -> connectedDirectoriesLimitReached) (\s@DirectoryLimits' {} a -> s {connectedDirectoriesLimitReached = a} :: DirectoryLimits)

-- | The current number of AWS Managed Microsoft AD directories in the
-- region.
directoryLimits_cloudOnlyMicrosoftADCurrentCount :: Lens.Lens' DirectoryLimits (Core.Maybe Core.Natural)
directoryLimits_cloudOnlyMicrosoftADCurrentCount = Lens.lens (\DirectoryLimits' {cloudOnlyMicrosoftADCurrentCount} -> cloudOnlyMicrosoftADCurrentCount) (\s@DirectoryLimits' {} a -> s {cloudOnlyMicrosoftADCurrentCount = a} :: DirectoryLimits)

-- | The maximum number of connected directories allowed in the Region.
directoryLimits_connectedDirectoriesLimit :: Lens.Lens' DirectoryLimits (Core.Maybe Core.Natural)
directoryLimits_connectedDirectoriesLimit = Lens.lens (\DirectoryLimits' {connectedDirectoriesLimit} -> connectedDirectoriesLimit) (\s@DirectoryLimits' {} a -> s {connectedDirectoriesLimit = a} :: DirectoryLimits)

-- | The current number of connected directories in the Region.
directoryLimits_connectedDirectoriesCurrentCount :: Lens.Lens' DirectoryLimits (Core.Maybe Core.Natural)
directoryLimits_connectedDirectoriesCurrentCount = Lens.lens (\DirectoryLimits' {connectedDirectoriesCurrentCount} -> connectedDirectoriesCurrentCount) (\s@DirectoryLimits' {} a -> s {connectedDirectoriesCurrentCount = a} :: DirectoryLimits)

-- | The maximum number of AWS Managed Microsoft AD directories allowed in
-- the region.
directoryLimits_cloudOnlyMicrosoftADLimit :: Lens.Lens' DirectoryLimits (Core.Maybe Core.Natural)
directoryLimits_cloudOnlyMicrosoftADLimit = Lens.lens (\DirectoryLimits' {cloudOnlyMicrosoftADLimit} -> cloudOnlyMicrosoftADLimit) (\s@DirectoryLimits' {} a -> s {cloudOnlyMicrosoftADLimit = a} :: DirectoryLimits)

-- | Indicates if the cloud directory limit has been reached.
directoryLimits_cloudOnlyDirectoriesLimitReached :: Lens.Lens' DirectoryLimits (Core.Maybe Core.Bool)
directoryLimits_cloudOnlyDirectoriesLimitReached = Lens.lens (\DirectoryLimits' {cloudOnlyDirectoriesLimitReached} -> cloudOnlyDirectoriesLimitReached) (\s@DirectoryLimits' {} a -> s {cloudOnlyDirectoriesLimitReached = a} :: DirectoryLimits)

-- | The current number of cloud directories in the Region.
directoryLimits_cloudOnlyDirectoriesCurrentCount :: Lens.Lens' DirectoryLimits (Core.Maybe Core.Natural)
directoryLimits_cloudOnlyDirectoriesCurrentCount = Lens.lens (\DirectoryLimits' {cloudOnlyDirectoriesCurrentCount} -> cloudOnlyDirectoriesCurrentCount) (\s@DirectoryLimits' {} a -> s {cloudOnlyDirectoriesCurrentCount = a} :: DirectoryLimits)

-- | Indicates if the AWS Managed Microsoft AD directory limit has been
-- reached.
directoryLimits_cloudOnlyMicrosoftADLimitReached :: Lens.Lens' DirectoryLimits (Core.Maybe Core.Bool)
directoryLimits_cloudOnlyMicrosoftADLimitReached = Lens.lens (\DirectoryLimits' {cloudOnlyMicrosoftADLimitReached} -> cloudOnlyMicrosoftADLimitReached) (\s@DirectoryLimits' {} a -> s {cloudOnlyMicrosoftADLimitReached = a} :: DirectoryLimits)

instance Core.FromJSON DirectoryLimits where
  parseJSON =
    Core.withObject
      "DirectoryLimits"
      ( \x ->
          DirectoryLimits'
            Core.<$> (x Core..:? "CloudOnlyDirectoriesLimit")
            Core.<*> (x Core..:? "ConnectedDirectoriesLimitReached")
            Core.<*> (x Core..:? "CloudOnlyMicrosoftADCurrentCount")
            Core.<*> (x Core..:? "ConnectedDirectoriesLimit")
            Core.<*> (x Core..:? "ConnectedDirectoriesCurrentCount")
            Core.<*> (x Core..:? "CloudOnlyMicrosoftADLimit")
            Core.<*> (x Core..:? "CloudOnlyDirectoriesLimitReached")
            Core.<*> (x Core..:? "CloudOnlyDirectoriesCurrentCount")
            Core.<*> (x Core..:? "CloudOnlyMicrosoftADLimitReached")
      )

instance Core.Hashable DirectoryLimits

instance Core.NFData DirectoryLimits
