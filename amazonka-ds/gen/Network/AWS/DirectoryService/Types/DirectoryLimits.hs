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
import qualified Network.AWS.Prelude as Prelude

-- | Contains directory limit information for a Region.
--
-- /See:/ 'newDirectoryLimits' smart constructor.
data DirectoryLimits = DirectoryLimits'
  { -- | The maximum number of cloud directories allowed in the Region.
    cloudOnlyDirectoriesLimit :: Prelude.Maybe Prelude.Natural,
    -- | Indicates if the connected directory limit has been reached.
    connectedDirectoriesLimitReached :: Prelude.Maybe Prelude.Bool,
    -- | The current number of AWS Managed Microsoft AD directories in the
    -- region.
    cloudOnlyMicrosoftADCurrentCount :: Prelude.Maybe Prelude.Natural,
    -- | The maximum number of connected directories allowed in the Region.
    connectedDirectoriesLimit :: Prelude.Maybe Prelude.Natural,
    -- | The current number of connected directories in the Region.
    connectedDirectoriesCurrentCount :: Prelude.Maybe Prelude.Natural,
    -- | The maximum number of AWS Managed Microsoft AD directories allowed in
    -- the region.
    cloudOnlyMicrosoftADLimit :: Prelude.Maybe Prelude.Natural,
    -- | Indicates if the cloud directory limit has been reached.
    cloudOnlyDirectoriesLimitReached :: Prelude.Maybe Prelude.Bool,
    -- | The current number of cloud directories in the Region.
    cloudOnlyDirectoriesCurrentCount :: Prelude.Maybe Prelude.Natural,
    -- | Indicates if the AWS Managed Microsoft AD directory limit has been
    -- reached.
    cloudOnlyMicrosoftADLimitReached :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
        Prelude.Nothing,
      connectedDirectoriesLimitReached = Prelude.Nothing,
      cloudOnlyMicrosoftADCurrentCount = Prelude.Nothing,
      connectedDirectoriesLimit = Prelude.Nothing,
      connectedDirectoriesCurrentCount = Prelude.Nothing,
      cloudOnlyMicrosoftADLimit = Prelude.Nothing,
      cloudOnlyDirectoriesLimitReached = Prelude.Nothing,
      cloudOnlyDirectoriesCurrentCount = Prelude.Nothing,
      cloudOnlyMicrosoftADLimitReached = Prelude.Nothing
    }

-- | The maximum number of cloud directories allowed in the Region.
directoryLimits_cloudOnlyDirectoriesLimit :: Lens.Lens' DirectoryLimits (Prelude.Maybe Prelude.Natural)
directoryLimits_cloudOnlyDirectoriesLimit = Lens.lens (\DirectoryLimits' {cloudOnlyDirectoriesLimit} -> cloudOnlyDirectoriesLimit) (\s@DirectoryLimits' {} a -> s {cloudOnlyDirectoriesLimit = a} :: DirectoryLimits)

-- | Indicates if the connected directory limit has been reached.
directoryLimits_connectedDirectoriesLimitReached :: Lens.Lens' DirectoryLimits (Prelude.Maybe Prelude.Bool)
directoryLimits_connectedDirectoriesLimitReached = Lens.lens (\DirectoryLimits' {connectedDirectoriesLimitReached} -> connectedDirectoriesLimitReached) (\s@DirectoryLimits' {} a -> s {connectedDirectoriesLimitReached = a} :: DirectoryLimits)

-- | The current number of AWS Managed Microsoft AD directories in the
-- region.
directoryLimits_cloudOnlyMicrosoftADCurrentCount :: Lens.Lens' DirectoryLimits (Prelude.Maybe Prelude.Natural)
directoryLimits_cloudOnlyMicrosoftADCurrentCount = Lens.lens (\DirectoryLimits' {cloudOnlyMicrosoftADCurrentCount} -> cloudOnlyMicrosoftADCurrentCount) (\s@DirectoryLimits' {} a -> s {cloudOnlyMicrosoftADCurrentCount = a} :: DirectoryLimits)

-- | The maximum number of connected directories allowed in the Region.
directoryLimits_connectedDirectoriesLimit :: Lens.Lens' DirectoryLimits (Prelude.Maybe Prelude.Natural)
directoryLimits_connectedDirectoriesLimit = Lens.lens (\DirectoryLimits' {connectedDirectoriesLimit} -> connectedDirectoriesLimit) (\s@DirectoryLimits' {} a -> s {connectedDirectoriesLimit = a} :: DirectoryLimits)

-- | The current number of connected directories in the Region.
directoryLimits_connectedDirectoriesCurrentCount :: Lens.Lens' DirectoryLimits (Prelude.Maybe Prelude.Natural)
directoryLimits_connectedDirectoriesCurrentCount = Lens.lens (\DirectoryLimits' {connectedDirectoriesCurrentCount} -> connectedDirectoriesCurrentCount) (\s@DirectoryLimits' {} a -> s {connectedDirectoriesCurrentCount = a} :: DirectoryLimits)

-- | The maximum number of AWS Managed Microsoft AD directories allowed in
-- the region.
directoryLimits_cloudOnlyMicrosoftADLimit :: Lens.Lens' DirectoryLimits (Prelude.Maybe Prelude.Natural)
directoryLimits_cloudOnlyMicrosoftADLimit = Lens.lens (\DirectoryLimits' {cloudOnlyMicrosoftADLimit} -> cloudOnlyMicrosoftADLimit) (\s@DirectoryLimits' {} a -> s {cloudOnlyMicrosoftADLimit = a} :: DirectoryLimits)

-- | Indicates if the cloud directory limit has been reached.
directoryLimits_cloudOnlyDirectoriesLimitReached :: Lens.Lens' DirectoryLimits (Prelude.Maybe Prelude.Bool)
directoryLimits_cloudOnlyDirectoriesLimitReached = Lens.lens (\DirectoryLimits' {cloudOnlyDirectoriesLimitReached} -> cloudOnlyDirectoriesLimitReached) (\s@DirectoryLimits' {} a -> s {cloudOnlyDirectoriesLimitReached = a} :: DirectoryLimits)

-- | The current number of cloud directories in the Region.
directoryLimits_cloudOnlyDirectoriesCurrentCount :: Lens.Lens' DirectoryLimits (Prelude.Maybe Prelude.Natural)
directoryLimits_cloudOnlyDirectoriesCurrentCount = Lens.lens (\DirectoryLimits' {cloudOnlyDirectoriesCurrentCount} -> cloudOnlyDirectoriesCurrentCount) (\s@DirectoryLimits' {} a -> s {cloudOnlyDirectoriesCurrentCount = a} :: DirectoryLimits)

-- | Indicates if the AWS Managed Microsoft AD directory limit has been
-- reached.
directoryLimits_cloudOnlyMicrosoftADLimitReached :: Lens.Lens' DirectoryLimits (Prelude.Maybe Prelude.Bool)
directoryLimits_cloudOnlyMicrosoftADLimitReached = Lens.lens (\DirectoryLimits' {cloudOnlyMicrosoftADLimitReached} -> cloudOnlyMicrosoftADLimitReached) (\s@DirectoryLimits' {} a -> s {cloudOnlyMicrosoftADLimitReached = a} :: DirectoryLimits)

instance Core.FromJSON DirectoryLimits where
  parseJSON =
    Core.withObject
      "DirectoryLimits"
      ( \x ->
          DirectoryLimits'
            Prelude.<$> (x Core..:? "CloudOnlyDirectoriesLimit")
            Prelude.<*> (x Core..:? "ConnectedDirectoriesLimitReached")
            Prelude.<*> (x Core..:? "CloudOnlyMicrosoftADCurrentCount")
            Prelude.<*> (x Core..:? "ConnectedDirectoriesLimit")
            Prelude.<*> (x Core..:? "ConnectedDirectoriesCurrentCount")
            Prelude.<*> (x Core..:? "CloudOnlyMicrosoftADLimit")
            Prelude.<*> (x Core..:? "CloudOnlyDirectoriesLimitReached")
            Prelude.<*> (x Core..:? "CloudOnlyDirectoriesCurrentCount")
            Prelude.<*> (x Core..:? "CloudOnlyMicrosoftADLimitReached")
      )

instance Prelude.Hashable DirectoryLimits

instance Prelude.NFData DirectoryLimits
