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
-- Module      : Amazonka.SecretsManager.Types.SecretVersionsListEntry
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecretsManager.Types.SecretVersionsListEntry where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A structure that contains information about one version of a secret.
--
-- /See:/ 'newSecretVersionsListEntry' smart constructor.
data SecretVersionsListEntry = SecretVersionsListEntry'
  { -- | The date and time this version of the secret was created.
    createdDate :: Prelude.Maybe Data.POSIX,
    -- | The KMS keys used to encrypt the secret version.
    kmsKeyIds :: Prelude.Maybe [Prelude.Text],
    -- | The date that this version of the secret was last accessed. Note that
    -- the resolution of this field is at the date level and does not include
    -- the time.
    lastAccessedDate :: Prelude.Maybe Data.POSIX,
    -- | The unique version identifier of this version of the secret.
    versionId :: Prelude.Maybe Prelude.Text,
    -- | An array of staging labels that are currently associated with this
    -- version of the secret.
    versionStages :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'kmsKeyIds', 'secretVersionsListEntry_kmsKeyIds' - The KMS keys used to encrypt the secret version.
--
-- 'lastAccessedDate', 'secretVersionsListEntry_lastAccessedDate' - The date that this version of the secret was last accessed. Note that
-- the resolution of this field is at the date level and does not include
-- the time.
--
-- 'versionId', 'secretVersionsListEntry_versionId' - The unique version identifier of this version of the secret.
--
-- 'versionStages', 'secretVersionsListEntry_versionStages' - An array of staging labels that are currently associated with this
-- version of the secret.
newSecretVersionsListEntry ::
  SecretVersionsListEntry
newSecretVersionsListEntry =
  SecretVersionsListEntry'
    { createdDate =
        Prelude.Nothing,
      kmsKeyIds = Prelude.Nothing,
      lastAccessedDate = Prelude.Nothing,
      versionId = Prelude.Nothing,
      versionStages = Prelude.Nothing
    }

-- | The date and time this version of the secret was created.
secretVersionsListEntry_createdDate :: Lens.Lens' SecretVersionsListEntry (Prelude.Maybe Prelude.UTCTime)
secretVersionsListEntry_createdDate = Lens.lens (\SecretVersionsListEntry' {createdDate} -> createdDate) (\s@SecretVersionsListEntry' {} a -> s {createdDate = a} :: SecretVersionsListEntry) Prelude.. Lens.mapping Data._Time

-- | The KMS keys used to encrypt the secret version.
secretVersionsListEntry_kmsKeyIds :: Lens.Lens' SecretVersionsListEntry (Prelude.Maybe [Prelude.Text])
secretVersionsListEntry_kmsKeyIds = Lens.lens (\SecretVersionsListEntry' {kmsKeyIds} -> kmsKeyIds) (\s@SecretVersionsListEntry' {} a -> s {kmsKeyIds = a} :: SecretVersionsListEntry) Prelude.. Lens.mapping Lens.coerced

-- | The date that this version of the secret was last accessed. Note that
-- the resolution of this field is at the date level and does not include
-- the time.
secretVersionsListEntry_lastAccessedDate :: Lens.Lens' SecretVersionsListEntry (Prelude.Maybe Prelude.UTCTime)
secretVersionsListEntry_lastAccessedDate = Lens.lens (\SecretVersionsListEntry' {lastAccessedDate} -> lastAccessedDate) (\s@SecretVersionsListEntry' {} a -> s {lastAccessedDate = a} :: SecretVersionsListEntry) Prelude.. Lens.mapping Data._Time

-- | The unique version identifier of this version of the secret.
secretVersionsListEntry_versionId :: Lens.Lens' SecretVersionsListEntry (Prelude.Maybe Prelude.Text)
secretVersionsListEntry_versionId = Lens.lens (\SecretVersionsListEntry' {versionId} -> versionId) (\s@SecretVersionsListEntry' {} a -> s {versionId = a} :: SecretVersionsListEntry)

-- | An array of staging labels that are currently associated with this
-- version of the secret.
secretVersionsListEntry_versionStages :: Lens.Lens' SecretVersionsListEntry (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
secretVersionsListEntry_versionStages = Lens.lens (\SecretVersionsListEntry' {versionStages} -> versionStages) (\s@SecretVersionsListEntry' {} a -> s {versionStages = a} :: SecretVersionsListEntry) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON SecretVersionsListEntry where
  parseJSON =
    Data.withObject
      "SecretVersionsListEntry"
      ( \x ->
          SecretVersionsListEntry'
            Prelude.<$> (x Data..:? "CreatedDate")
            Prelude.<*> (x Data..:? "KmsKeyIds" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "LastAccessedDate")
            Prelude.<*> (x Data..:? "VersionId")
            Prelude.<*> (x Data..:? "VersionStages")
      )

instance Prelude.Hashable SecretVersionsListEntry where
  hashWithSalt _salt SecretVersionsListEntry' {..} =
    _salt `Prelude.hashWithSalt` createdDate
      `Prelude.hashWithSalt` kmsKeyIds
      `Prelude.hashWithSalt` lastAccessedDate
      `Prelude.hashWithSalt` versionId
      `Prelude.hashWithSalt` versionStages

instance Prelude.NFData SecretVersionsListEntry where
  rnf SecretVersionsListEntry' {..} =
    Prelude.rnf createdDate
      `Prelude.seq` Prelude.rnf kmsKeyIds
      `Prelude.seq` Prelude.rnf lastAccessedDate
      `Prelude.seq` Prelude.rnf versionId
      `Prelude.seq` Prelude.rnf versionStages
