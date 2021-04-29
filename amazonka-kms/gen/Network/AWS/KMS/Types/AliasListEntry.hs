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
-- Module      : Network.AWS.KMS.Types.AliasListEntry
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KMS.Types.AliasListEntry where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains information about an alias.
--
-- /See:/ 'newAliasListEntry' smart constructor.
data AliasListEntry = AliasListEntry'
  { lastUpdatedDate :: Prelude.Maybe Prelude.POSIX,
    creationDate :: Prelude.Maybe Prelude.POSIX,
    -- | String that contains the alias. This value begins with @alias\/@.
    aliasName :: Prelude.Maybe Prelude.Text,
    -- | String that contains the key ARN.
    aliasArn :: Prelude.Maybe Prelude.Text,
    -- | String that contains the key identifier referred to by the alias.
    targetKeyId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AliasListEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastUpdatedDate', 'aliasListEntry_lastUpdatedDate' - Undocumented member.
--
-- 'creationDate', 'aliasListEntry_creationDate' - Undocumented member.
--
-- 'aliasName', 'aliasListEntry_aliasName' - String that contains the alias. This value begins with @alias\/@.
--
-- 'aliasArn', 'aliasListEntry_aliasArn' - String that contains the key ARN.
--
-- 'targetKeyId', 'aliasListEntry_targetKeyId' - String that contains the key identifier referred to by the alias.
newAliasListEntry ::
  AliasListEntry
newAliasListEntry =
  AliasListEntry'
    { lastUpdatedDate = Prelude.Nothing,
      creationDate = Prelude.Nothing,
      aliasName = Prelude.Nothing,
      aliasArn = Prelude.Nothing,
      targetKeyId = Prelude.Nothing
    }

-- | Undocumented member.
aliasListEntry_lastUpdatedDate :: Lens.Lens' AliasListEntry (Prelude.Maybe Prelude.UTCTime)
aliasListEntry_lastUpdatedDate = Lens.lens (\AliasListEntry' {lastUpdatedDate} -> lastUpdatedDate) (\s@AliasListEntry' {} a -> s {lastUpdatedDate = a} :: AliasListEntry) Prelude.. Lens.mapping Prelude._Time

-- | Undocumented member.
aliasListEntry_creationDate :: Lens.Lens' AliasListEntry (Prelude.Maybe Prelude.UTCTime)
aliasListEntry_creationDate = Lens.lens (\AliasListEntry' {creationDate} -> creationDate) (\s@AliasListEntry' {} a -> s {creationDate = a} :: AliasListEntry) Prelude.. Lens.mapping Prelude._Time

-- | String that contains the alias. This value begins with @alias\/@.
aliasListEntry_aliasName :: Lens.Lens' AliasListEntry (Prelude.Maybe Prelude.Text)
aliasListEntry_aliasName = Lens.lens (\AliasListEntry' {aliasName} -> aliasName) (\s@AliasListEntry' {} a -> s {aliasName = a} :: AliasListEntry)

-- | String that contains the key ARN.
aliasListEntry_aliasArn :: Lens.Lens' AliasListEntry (Prelude.Maybe Prelude.Text)
aliasListEntry_aliasArn = Lens.lens (\AliasListEntry' {aliasArn} -> aliasArn) (\s@AliasListEntry' {} a -> s {aliasArn = a} :: AliasListEntry)

-- | String that contains the key identifier referred to by the alias.
aliasListEntry_targetKeyId :: Lens.Lens' AliasListEntry (Prelude.Maybe Prelude.Text)
aliasListEntry_targetKeyId = Lens.lens (\AliasListEntry' {targetKeyId} -> targetKeyId) (\s@AliasListEntry' {} a -> s {targetKeyId = a} :: AliasListEntry)

instance Prelude.FromJSON AliasListEntry where
  parseJSON =
    Prelude.withObject
      "AliasListEntry"
      ( \x ->
          AliasListEntry'
            Prelude.<$> (x Prelude..:? "LastUpdatedDate")
            Prelude.<*> (x Prelude..:? "CreationDate")
            Prelude.<*> (x Prelude..:? "AliasName")
            Prelude.<*> (x Prelude..:? "AliasArn")
            Prelude.<*> (x Prelude..:? "TargetKeyId")
      )

instance Prelude.Hashable AliasListEntry

instance Prelude.NFData AliasListEntry
