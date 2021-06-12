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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Contains information about an alias.
--
-- /See:/ 'newAliasListEntry' smart constructor.
data AliasListEntry = AliasListEntry'
  { lastUpdatedDate :: Core.Maybe Core.POSIX,
    creationDate :: Core.Maybe Core.POSIX,
    -- | String that contains the alias. This value begins with @alias\/@.
    aliasName :: Core.Maybe Core.Text,
    -- | String that contains the key ARN.
    aliasArn :: Core.Maybe Core.Text,
    -- | String that contains the key identifier referred to by the alias.
    targetKeyId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { lastUpdatedDate = Core.Nothing,
      creationDate = Core.Nothing,
      aliasName = Core.Nothing,
      aliasArn = Core.Nothing,
      targetKeyId = Core.Nothing
    }

-- | Undocumented member.
aliasListEntry_lastUpdatedDate :: Lens.Lens' AliasListEntry (Core.Maybe Core.UTCTime)
aliasListEntry_lastUpdatedDate = Lens.lens (\AliasListEntry' {lastUpdatedDate} -> lastUpdatedDate) (\s@AliasListEntry' {} a -> s {lastUpdatedDate = a} :: AliasListEntry) Core.. Lens.mapping Core._Time

-- | Undocumented member.
aliasListEntry_creationDate :: Lens.Lens' AliasListEntry (Core.Maybe Core.UTCTime)
aliasListEntry_creationDate = Lens.lens (\AliasListEntry' {creationDate} -> creationDate) (\s@AliasListEntry' {} a -> s {creationDate = a} :: AliasListEntry) Core.. Lens.mapping Core._Time

-- | String that contains the alias. This value begins with @alias\/@.
aliasListEntry_aliasName :: Lens.Lens' AliasListEntry (Core.Maybe Core.Text)
aliasListEntry_aliasName = Lens.lens (\AliasListEntry' {aliasName} -> aliasName) (\s@AliasListEntry' {} a -> s {aliasName = a} :: AliasListEntry)

-- | String that contains the key ARN.
aliasListEntry_aliasArn :: Lens.Lens' AliasListEntry (Core.Maybe Core.Text)
aliasListEntry_aliasArn = Lens.lens (\AliasListEntry' {aliasArn} -> aliasArn) (\s@AliasListEntry' {} a -> s {aliasArn = a} :: AliasListEntry)

-- | String that contains the key identifier referred to by the alias.
aliasListEntry_targetKeyId :: Lens.Lens' AliasListEntry (Core.Maybe Core.Text)
aliasListEntry_targetKeyId = Lens.lens (\AliasListEntry' {targetKeyId} -> targetKeyId) (\s@AliasListEntry' {} a -> s {targetKeyId = a} :: AliasListEntry)

instance Core.FromJSON AliasListEntry where
  parseJSON =
    Core.withObject
      "AliasListEntry"
      ( \x ->
          AliasListEntry'
            Core.<$> (x Core..:? "LastUpdatedDate")
            Core.<*> (x Core..:? "CreationDate")
            Core.<*> (x Core..:? "AliasName")
            Core.<*> (x Core..:? "AliasArn")
            Core.<*> (x Core..:? "TargetKeyId")
      )

instance Core.Hashable AliasListEntry

instance Core.NFData AliasListEntry
