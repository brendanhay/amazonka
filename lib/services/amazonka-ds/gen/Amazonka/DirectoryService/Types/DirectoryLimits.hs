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
-- Module      : Amazonka.DirectoryService.Types.DirectoryLimits
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DirectoryService.Types.DirectoryLimits where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains directory limit information for a Region.
--
-- /See:/ 'newDirectoryLimits' smart constructor.
data DirectoryLimits = DirectoryLimits'
  { -- | The current number of cloud directories in the Region.
    cloudOnlyDirectoriesCurrentCount :: Prelude.Maybe Prelude.Natural,
    -- | The maximum number of cloud directories allowed in the Region.
    cloudOnlyDirectoriesLimit :: Prelude.Maybe Prelude.Natural,
    -- | Indicates if the cloud directory limit has been reached.
    cloudOnlyDirectoriesLimitReached :: Prelude.Maybe Prelude.Bool,
    -- | The current number of Managed Microsoft AD directories in the region.
    cloudOnlyMicrosoftADCurrentCount :: Prelude.Maybe Prelude.Natural,
    -- | The maximum number of Managed Microsoft AD directories allowed in the
    -- region.
    cloudOnlyMicrosoftADLimit :: Prelude.Maybe Prelude.Natural,
    -- | Indicates if the Managed Microsoft AD directory limit has been reached.
    cloudOnlyMicrosoftADLimitReached :: Prelude.Maybe Prelude.Bool,
    -- | The current number of connected directories in the Region.
    connectedDirectoriesCurrentCount :: Prelude.Maybe Prelude.Natural,
    -- | The maximum number of connected directories allowed in the Region.
    connectedDirectoriesLimit :: Prelude.Maybe Prelude.Natural,
    -- | Indicates if the connected directory limit has been reached.
    connectedDirectoriesLimitReached :: Prelude.Maybe Prelude.Bool
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
-- 'cloudOnlyDirectoriesCurrentCount', 'directoryLimits_cloudOnlyDirectoriesCurrentCount' - The current number of cloud directories in the Region.
--
-- 'cloudOnlyDirectoriesLimit', 'directoryLimits_cloudOnlyDirectoriesLimit' - The maximum number of cloud directories allowed in the Region.
--
-- 'cloudOnlyDirectoriesLimitReached', 'directoryLimits_cloudOnlyDirectoriesLimitReached' - Indicates if the cloud directory limit has been reached.
--
-- 'cloudOnlyMicrosoftADCurrentCount', 'directoryLimits_cloudOnlyMicrosoftADCurrentCount' - The current number of Managed Microsoft AD directories in the region.
--
-- 'cloudOnlyMicrosoftADLimit', 'directoryLimits_cloudOnlyMicrosoftADLimit' - The maximum number of Managed Microsoft AD directories allowed in the
-- region.
--
-- 'cloudOnlyMicrosoftADLimitReached', 'directoryLimits_cloudOnlyMicrosoftADLimitReached' - Indicates if the Managed Microsoft AD directory limit has been reached.
--
-- 'connectedDirectoriesCurrentCount', 'directoryLimits_connectedDirectoriesCurrentCount' - The current number of connected directories in the Region.
--
-- 'connectedDirectoriesLimit', 'directoryLimits_connectedDirectoriesLimit' - The maximum number of connected directories allowed in the Region.
--
-- 'connectedDirectoriesLimitReached', 'directoryLimits_connectedDirectoriesLimitReached' - Indicates if the connected directory limit has been reached.
newDirectoryLimits ::
  DirectoryLimits
newDirectoryLimits =
  DirectoryLimits'
    { cloudOnlyDirectoriesCurrentCount =
        Prelude.Nothing,
      cloudOnlyDirectoriesLimit = Prelude.Nothing,
      cloudOnlyDirectoriesLimitReached = Prelude.Nothing,
      cloudOnlyMicrosoftADCurrentCount = Prelude.Nothing,
      cloudOnlyMicrosoftADLimit = Prelude.Nothing,
      cloudOnlyMicrosoftADLimitReached = Prelude.Nothing,
      connectedDirectoriesCurrentCount = Prelude.Nothing,
      connectedDirectoriesLimit = Prelude.Nothing,
      connectedDirectoriesLimitReached = Prelude.Nothing
    }

-- | The current number of cloud directories in the Region.
directoryLimits_cloudOnlyDirectoriesCurrentCount :: Lens.Lens' DirectoryLimits (Prelude.Maybe Prelude.Natural)
directoryLimits_cloudOnlyDirectoriesCurrentCount = Lens.lens (\DirectoryLimits' {cloudOnlyDirectoriesCurrentCount} -> cloudOnlyDirectoriesCurrentCount) (\s@DirectoryLimits' {} a -> s {cloudOnlyDirectoriesCurrentCount = a} :: DirectoryLimits)

-- | The maximum number of cloud directories allowed in the Region.
directoryLimits_cloudOnlyDirectoriesLimit :: Lens.Lens' DirectoryLimits (Prelude.Maybe Prelude.Natural)
directoryLimits_cloudOnlyDirectoriesLimit = Lens.lens (\DirectoryLimits' {cloudOnlyDirectoriesLimit} -> cloudOnlyDirectoriesLimit) (\s@DirectoryLimits' {} a -> s {cloudOnlyDirectoriesLimit = a} :: DirectoryLimits)

-- | Indicates if the cloud directory limit has been reached.
directoryLimits_cloudOnlyDirectoriesLimitReached :: Lens.Lens' DirectoryLimits (Prelude.Maybe Prelude.Bool)
directoryLimits_cloudOnlyDirectoriesLimitReached = Lens.lens (\DirectoryLimits' {cloudOnlyDirectoriesLimitReached} -> cloudOnlyDirectoriesLimitReached) (\s@DirectoryLimits' {} a -> s {cloudOnlyDirectoriesLimitReached = a} :: DirectoryLimits)

-- | The current number of Managed Microsoft AD directories in the region.
directoryLimits_cloudOnlyMicrosoftADCurrentCount :: Lens.Lens' DirectoryLimits (Prelude.Maybe Prelude.Natural)
directoryLimits_cloudOnlyMicrosoftADCurrentCount = Lens.lens (\DirectoryLimits' {cloudOnlyMicrosoftADCurrentCount} -> cloudOnlyMicrosoftADCurrentCount) (\s@DirectoryLimits' {} a -> s {cloudOnlyMicrosoftADCurrentCount = a} :: DirectoryLimits)

-- | The maximum number of Managed Microsoft AD directories allowed in the
-- region.
directoryLimits_cloudOnlyMicrosoftADLimit :: Lens.Lens' DirectoryLimits (Prelude.Maybe Prelude.Natural)
directoryLimits_cloudOnlyMicrosoftADLimit = Lens.lens (\DirectoryLimits' {cloudOnlyMicrosoftADLimit} -> cloudOnlyMicrosoftADLimit) (\s@DirectoryLimits' {} a -> s {cloudOnlyMicrosoftADLimit = a} :: DirectoryLimits)

-- | Indicates if the Managed Microsoft AD directory limit has been reached.
directoryLimits_cloudOnlyMicrosoftADLimitReached :: Lens.Lens' DirectoryLimits (Prelude.Maybe Prelude.Bool)
directoryLimits_cloudOnlyMicrosoftADLimitReached = Lens.lens (\DirectoryLimits' {cloudOnlyMicrosoftADLimitReached} -> cloudOnlyMicrosoftADLimitReached) (\s@DirectoryLimits' {} a -> s {cloudOnlyMicrosoftADLimitReached = a} :: DirectoryLimits)

-- | The current number of connected directories in the Region.
directoryLimits_connectedDirectoriesCurrentCount :: Lens.Lens' DirectoryLimits (Prelude.Maybe Prelude.Natural)
directoryLimits_connectedDirectoriesCurrentCount = Lens.lens (\DirectoryLimits' {connectedDirectoriesCurrentCount} -> connectedDirectoriesCurrentCount) (\s@DirectoryLimits' {} a -> s {connectedDirectoriesCurrentCount = a} :: DirectoryLimits)

-- | The maximum number of connected directories allowed in the Region.
directoryLimits_connectedDirectoriesLimit :: Lens.Lens' DirectoryLimits (Prelude.Maybe Prelude.Natural)
directoryLimits_connectedDirectoriesLimit = Lens.lens (\DirectoryLimits' {connectedDirectoriesLimit} -> connectedDirectoriesLimit) (\s@DirectoryLimits' {} a -> s {connectedDirectoriesLimit = a} :: DirectoryLimits)

-- | Indicates if the connected directory limit has been reached.
directoryLimits_connectedDirectoriesLimitReached :: Lens.Lens' DirectoryLimits (Prelude.Maybe Prelude.Bool)
directoryLimits_connectedDirectoriesLimitReached = Lens.lens (\DirectoryLimits' {connectedDirectoriesLimitReached} -> connectedDirectoriesLimitReached) (\s@DirectoryLimits' {} a -> s {connectedDirectoriesLimitReached = a} :: DirectoryLimits)

instance Data.FromJSON DirectoryLimits where
  parseJSON =
    Data.withObject
      "DirectoryLimits"
      ( \x ->
          DirectoryLimits'
            Prelude.<$> (x Data..:? "CloudOnlyDirectoriesCurrentCount")
            Prelude.<*> (x Data..:? "CloudOnlyDirectoriesLimit")
            Prelude.<*> (x Data..:? "CloudOnlyDirectoriesLimitReached")
            Prelude.<*> (x Data..:? "CloudOnlyMicrosoftADCurrentCount")
            Prelude.<*> (x Data..:? "CloudOnlyMicrosoftADLimit")
            Prelude.<*> (x Data..:? "CloudOnlyMicrosoftADLimitReached")
            Prelude.<*> (x Data..:? "ConnectedDirectoriesCurrentCount")
            Prelude.<*> (x Data..:? "ConnectedDirectoriesLimit")
            Prelude.<*> (x Data..:? "ConnectedDirectoriesLimitReached")
      )

instance Prelude.Hashable DirectoryLimits where
  hashWithSalt _salt DirectoryLimits' {..} =
    _salt
      `Prelude.hashWithSalt` cloudOnlyDirectoriesCurrentCount
      `Prelude.hashWithSalt` cloudOnlyDirectoriesLimit
      `Prelude.hashWithSalt` cloudOnlyDirectoriesLimitReached
      `Prelude.hashWithSalt` cloudOnlyMicrosoftADCurrentCount
      `Prelude.hashWithSalt` cloudOnlyMicrosoftADLimit
      `Prelude.hashWithSalt` cloudOnlyMicrosoftADLimitReached
      `Prelude.hashWithSalt` connectedDirectoriesCurrentCount
      `Prelude.hashWithSalt` connectedDirectoriesLimit
      `Prelude.hashWithSalt` connectedDirectoriesLimitReached

instance Prelude.NFData DirectoryLimits where
  rnf DirectoryLimits' {..} =
    Prelude.rnf cloudOnlyDirectoriesCurrentCount
      `Prelude.seq` Prelude.rnf cloudOnlyDirectoriesLimit
      `Prelude.seq` Prelude.rnf cloudOnlyDirectoriesLimitReached
      `Prelude.seq` Prelude.rnf cloudOnlyMicrosoftADCurrentCount
      `Prelude.seq` Prelude.rnf cloudOnlyMicrosoftADLimit
      `Prelude.seq` Prelude.rnf cloudOnlyMicrosoftADLimitReached
      `Prelude.seq` Prelude.rnf connectedDirectoriesCurrentCount
      `Prelude.seq` Prelude.rnf connectedDirectoriesLimit
      `Prelude.seq` Prelude.rnf connectedDirectoriesLimitReached
