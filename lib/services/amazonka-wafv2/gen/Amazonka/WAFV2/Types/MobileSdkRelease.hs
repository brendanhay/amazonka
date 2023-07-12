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
-- Module      : Amazonka.WAFV2.Types.MobileSdkRelease
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WAFV2.Types.MobileSdkRelease where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.WAFV2.Types.Tag

-- | Information for a release of the mobile SDK, including release notes and
-- tags.
--
-- The mobile SDK is not generally available. Customers who have access to
-- the mobile SDK can use it to establish and manage WAF tokens for use in
-- HTTP(S) requests from a mobile device to WAF. For more information, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-application-integration.html WAF client application integration>
-- in the /WAF Developer Guide/.
--
-- /See:/ 'newMobileSdkRelease' smart constructor.
data MobileSdkRelease = MobileSdkRelease'
  { -- | Notes describing the release.
    releaseNotes :: Prelude.Maybe Prelude.Text,
    -- | The release version.
    releaseVersion :: Prelude.Maybe Prelude.Text,
    -- | Tags that are associated with the release.
    tags :: Prelude.Maybe (Prelude.NonEmpty Tag),
    -- | The timestamp of the release.
    timestamp :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MobileSdkRelease' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'releaseNotes', 'mobileSdkRelease_releaseNotes' - Notes describing the release.
--
-- 'releaseVersion', 'mobileSdkRelease_releaseVersion' - The release version.
--
-- 'tags', 'mobileSdkRelease_tags' - Tags that are associated with the release.
--
-- 'timestamp', 'mobileSdkRelease_timestamp' - The timestamp of the release.
newMobileSdkRelease ::
  MobileSdkRelease
newMobileSdkRelease =
  MobileSdkRelease'
    { releaseNotes = Prelude.Nothing,
      releaseVersion = Prelude.Nothing,
      tags = Prelude.Nothing,
      timestamp = Prelude.Nothing
    }

-- | Notes describing the release.
mobileSdkRelease_releaseNotes :: Lens.Lens' MobileSdkRelease (Prelude.Maybe Prelude.Text)
mobileSdkRelease_releaseNotes = Lens.lens (\MobileSdkRelease' {releaseNotes} -> releaseNotes) (\s@MobileSdkRelease' {} a -> s {releaseNotes = a} :: MobileSdkRelease)

-- | The release version.
mobileSdkRelease_releaseVersion :: Lens.Lens' MobileSdkRelease (Prelude.Maybe Prelude.Text)
mobileSdkRelease_releaseVersion = Lens.lens (\MobileSdkRelease' {releaseVersion} -> releaseVersion) (\s@MobileSdkRelease' {} a -> s {releaseVersion = a} :: MobileSdkRelease)

-- | Tags that are associated with the release.
mobileSdkRelease_tags :: Lens.Lens' MobileSdkRelease (Prelude.Maybe (Prelude.NonEmpty Tag))
mobileSdkRelease_tags = Lens.lens (\MobileSdkRelease' {tags} -> tags) (\s@MobileSdkRelease' {} a -> s {tags = a} :: MobileSdkRelease) Prelude.. Lens.mapping Lens.coerced

-- | The timestamp of the release.
mobileSdkRelease_timestamp :: Lens.Lens' MobileSdkRelease (Prelude.Maybe Prelude.UTCTime)
mobileSdkRelease_timestamp = Lens.lens (\MobileSdkRelease' {timestamp} -> timestamp) (\s@MobileSdkRelease' {} a -> s {timestamp = a} :: MobileSdkRelease) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON MobileSdkRelease where
  parseJSON =
    Data.withObject
      "MobileSdkRelease"
      ( \x ->
          MobileSdkRelease'
            Prelude.<$> (x Data..:? "ReleaseNotes")
            Prelude.<*> (x Data..:? "ReleaseVersion")
            Prelude.<*> (x Data..:? "Tags")
            Prelude.<*> (x Data..:? "Timestamp")
      )

instance Prelude.Hashable MobileSdkRelease where
  hashWithSalt _salt MobileSdkRelease' {..} =
    _salt
      `Prelude.hashWithSalt` releaseNotes
      `Prelude.hashWithSalt` releaseVersion
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` timestamp

instance Prelude.NFData MobileSdkRelease where
  rnf MobileSdkRelease' {..} =
    Prelude.rnf releaseNotes
      `Prelude.seq` Prelude.rnf releaseVersion
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf timestamp
