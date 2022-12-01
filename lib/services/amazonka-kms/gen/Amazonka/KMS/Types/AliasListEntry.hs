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
-- Module      : Amazonka.KMS.Types.AliasListEntry
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KMS.Types.AliasListEntry where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains information about an alias.
--
-- /See:/ 'newAliasListEntry' smart constructor.
data AliasListEntry = AliasListEntry'
  { -- | Date and time that the alias was most recently associated with a KMS key
    -- in the account and Region. Formatted as Unix time.
    lastUpdatedDate :: Prelude.Maybe Core.POSIX,
    -- | String that contains the key ARN.
    aliasArn :: Prelude.Maybe Prelude.Text,
    -- | Date and time that the alias was most recently created in the account
    -- and Region. Formatted as Unix time.
    creationDate :: Prelude.Maybe Core.POSIX,
    -- | String that contains the key identifier of the KMS key associated with
    -- the alias.
    targetKeyId :: Prelude.Maybe Prelude.Text,
    -- | String that contains the alias. This value begins with @alias\/@.
    aliasName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AliasListEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastUpdatedDate', 'aliasListEntry_lastUpdatedDate' - Date and time that the alias was most recently associated with a KMS key
-- in the account and Region. Formatted as Unix time.
--
-- 'aliasArn', 'aliasListEntry_aliasArn' - String that contains the key ARN.
--
-- 'creationDate', 'aliasListEntry_creationDate' - Date and time that the alias was most recently created in the account
-- and Region. Formatted as Unix time.
--
-- 'targetKeyId', 'aliasListEntry_targetKeyId' - String that contains the key identifier of the KMS key associated with
-- the alias.
--
-- 'aliasName', 'aliasListEntry_aliasName' - String that contains the alias. This value begins with @alias\/@.
newAliasListEntry ::
  AliasListEntry
newAliasListEntry =
  AliasListEntry'
    { lastUpdatedDate = Prelude.Nothing,
      aliasArn = Prelude.Nothing,
      creationDate = Prelude.Nothing,
      targetKeyId = Prelude.Nothing,
      aliasName = Prelude.Nothing
    }

-- | Date and time that the alias was most recently associated with a KMS key
-- in the account and Region. Formatted as Unix time.
aliasListEntry_lastUpdatedDate :: Lens.Lens' AliasListEntry (Prelude.Maybe Prelude.UTCTime)
aliasListEntry_lastUpdatedDate = Lens.lens (\AliasListEntry' {lastUpdatedDate} -> lastUpdatedDate) (\s@AliasListEntry' {} a -> s {lastUpdatedDate = a} :: AliasListEntry) Prelude.. Lens.mapping Core._Time

-- | String that contains the key ARN.
aliasListEntry_aliasArn :: Lens.Lens' AliasListEntry (Prelude.Maybe Prelude.Text)
aliasListEntry_aliasArn = Lens.lens (\AliasListEntry' {aliasArn} -> aliasArn) (\s@AliasListEntry' {} a -> s {aliasArn = a} :: AliasListEntry)

-- | Date and time that the alias was most recently created in the account
-- and Region. Formatted as Unix time.
aliasListEntry_creationDate :: Lens.Lens' AliasListEntry (Prelude.Maybe Prelude.UTCTime)
aliasListEntry_creationDate = Lens.lens (\AliasListEntry' {creationDate} -> creationDate) (\s@AliasListEntry' {} a -> s {creationDate = a} :: AliasListEntry) Prelude.. Lens.mapping Core._Time

-- | String that contains the key identifier of the KMS key associated with
-- the alias.
aliasListEntry_targetKeyId :: Lens.Lens' AliasListEntry (Prelude.Maybe Prelude.Text)
aliasListEntry_targetKeyId = Lens.lens (\AliasListEntry' {targetKeyId} -> targetKeyId) (\s@AliasListEntry' {} a -> s {targetKeyId = a} :: AliasListEntry)

-- | String that contains the alias. This value begins with @alias\/@.
aliasListEntry_aliasName :: Lens.Lens' AliasListEntry (Prelude.Maybe Prelude.Text)
aliasListEntry_aliasName = Lens.lens (\AliasListEntry' {aliasName} -> aliasName) (\s@AliasListEntry' {} a -> s {aliasName = a} :: AliasListEntry)

instance Core.FromJSON AliasListEntry where
  parseJSON =
    Core.withObject
      "AliasListEntry"
      ( \x ->
          AliasListEntry'
            Prelude.<$> (x Core..:? "LastUpdatedDate")
            Prelude.<*> (x Core..:? "AliasArn")
            Prelude.<*> (x Core..:? "CreationDate")
            Prelude.<*> (x Core..:? "TargetKeyId")
            Prelude.<*> (x Core..:? "AliasName")
      )

instance Prelude.Hashable AliasListEntry where
  hashWithSalt _salt AliasListEntry' {..} =
    _salt `Prelude.hashWithSalt` lastUpdatedDate
      `Prelude.hashWithSalt` aliasArn
      `Prelude.hashWithSalt` creationDate
      `Prelude.hashWithSalt` targetKeyId
      `Prelude.hashWithSalt` aliasName

instance Prelude.NFData AliasListEntry where
  rnf AliasListEntry' {..} =
    Prelude.rnf lastUpdatedDate
      `Prelude.seq` Prelude.rnf aliasArn
      `Prelude.seq` Prelude.rnf creationDate
      `Prelude.seq` Prelude.rnf targetKeyId
      `Prelude.seq` Prelude.rnf aliasName
