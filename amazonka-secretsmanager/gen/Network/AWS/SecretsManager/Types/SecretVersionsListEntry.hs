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
-- Module      : Network.AWS.SecretsManager.Types.SecretVersionsListEntry
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SecretsManager.Types.SecretVersionsListEntry where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A structure that contains information about one version of a secret.
--
-- /See:/ 'newSecretVersionsListEntry' smart constructor.
data SecretVersionsListEntry = SecretVersionsListEntry'
  { -- | The date and time this version of the secret was created.
    createdDate :: Core.Maybe Core.POSIX,
    -- | An array of staging labels that are currently associated with this
    -- version of the secret.
    versionStages :: Core.Maybe (Core.NonEmpty Core.Text),
    -- | The unique version identifier of this version of the secret.
    versionId :: Core.Maybe Core.Text,
    -- | The date that this version of the secret was last accessed. Note that
    -- the resolution of this field is at the date level and does not include
    -- the time.
    lastAccessedDate :: Core.Maybe Core.POSIX
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SecretVersionsListEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdDate', 'secretVersionsListEntry_createdDate' - The date and time this version of the secret was created.
--
-- 'versionStages', 'secretVersionsListEntry_versionStages' - An array of staging labels that are currently associated with this
-- version of the secret.
--
-- 'versionId', 'secretVersionsListEntry_versionId' - The unique version identifier of this version of the secret.
--
-- 'lastAccessedDate', 'secretVersionsListEntry_lastAccessedDate' - The date that this version of the secret was last accessed. Note that
-- the resolution of this field is at the date level and does not include
-- the time.
newSecretVersionsListEntry ::
  SecretVersionsListEntry
newSecretVersionsListEntry =
  SecretVersionsListEntry'
    { createdDate =
        Core.Nothing,
      versionStages = Core.Nothing,
      versionId = Core.Nothing,
      lastAccessedDate = Core.Nothing
    }

-- | The date and time this version of the secret was created.
secretVersionsListEntry_createdDate :: Lens.Lens' SecretVersionsListEntry (Core.Maybe Core.UTCTime)
secretVersionsListEntry_createdDate = Lens.lens (\SecretVersionsListEntry' {createdDate} -> createdDate) (\s@SecretVersionsListEntry' {} a -> s {createdDate = a} :: SecretVersionsListEntry) Core.. Lens.mapping Core._Time

-- | An array of staging labels that are currently associated with this
-- version of the secret.
secretVersionsListEntry_versionStages :: Lens.Lens' SecretVersionsListEntry (Core.Maybe (Core.NonEmpty Core.Text))
secretVersionsListEntry_versionStages = Lens.lens (\SecretVersionsListEntry' {versionStages} -> versionStages) (\s@SecretVersionsListEntry' {} a -> s {versionStages = a} :: SecretVersionsListEntry) Core.. Lens.mapping Lens._Coerce

-- | The unique version identifier of this version of the secret.
secretVersionsListEntry_versionId :: Lens.Lens' SecretVersionsListEntry (Core.Maybe Core.Text)
secretVersionsListEntry_versionId = Lens.lens (\SecretVersionsListEntry' {versionId} -> versionId) (\s@SecretVersionsListEntry' {} a -> s {versionId = a} :: SecretVersionsListEntry)

-- | The date that this version of the secret was last accessed. Note that
-- the resolution of this field is at the date level and does not include
-- the time.
secretVersionsListEntry_lastAccessedDate :: Lens.Lens' SecretVersionsListEntry (Core.Maybe Core.UTCTime)
secretVersionsListEntry_lastAccessedDate = Lens.lens (\SecretVersionsListEntry' {lastAccessedDate} -> lastAccessedDate) (\s@SecretVersionsListEntry' {} a -> s {lastAccessedDate = a} :: SecretVersionsListEntry) Core.. Lens.mapping Core._Time

instance Core.FromJSON SecretVersionsListEntry where
  parseJSON =
    Core.withObject
      "SecretVersionsListEntry"
      ( \x ->
          SecretVersionsListEntry'
            Core.<$> (x Core..:? "CreatedDate")
            Core.<*> (x Core..:? "VersionStages")
            Core.<*> (x Core..:? "VersionId")
            Core.<*> (x Core..:? "LastAccessedDate")
      )

instance Core.Hashable SecretVersionsListEntry

instance Core.NFData SecretVersionsListEntry
