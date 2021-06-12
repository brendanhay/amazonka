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
-- Module      : Network.AWS.CognitoIdentity.Types.IdentityDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentity.Types.IdentityDescription where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A description of the identity.
--
-- /See:/ 'newIdentityDescription' smart constructor.
data IdentityDescription = IdentityDescription'
  { -- | Date on which the identity was last modified.
    lastModifiedDate :: Core.Maybe Core.POSIX,
    -- | Date on which the identity was created.
    creationDate :: Core.Maybe Core.POSIX,
    -- | A unique identifier in the format REGION:GUID.
    identityId :: Core.Maybe Core.Text,
    -- | The provider names.
    logins :: Core.Maybe [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'IdentityDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastModifiedDate', 'identityDescription_lastModifiedDate' - Date on which the identity was last modified.
--
-- 'creationDate', 'identityDescription_creationDate' - Date on which the identity was created.
--
-- 'identityId', 'identityDescription_identityId' - A unique identifier in the format REGION:GUID.
--
-- 'logins', 'identityDescription_logins' - The provider names.
newIdentityDescription ::
  IdentityDescription
newIdentityDescription =
  IdentityDescription'
    { lastModifiedDate =
        Core.Nothing,
      creationDate = Core.Nothing,
      identityId = Core.Nothing,
      logins = Core.Nothing
    }

-- | Date on which the identity was last modified.
identityDescription_lastModifiedDate :: Lens.Lens' IdentityDescription (Core.Maybe Core.UTCTime)
identityDescription_lastModifiedDate = Lens.lens (\IdentityDescription' {lastModifiedDate} -> lastModifiedDate) (\s@IdentityDescription' {} a -> s {lastModifiedDate = a} :: IdentityDescription) Core.. Lens.mapping Core._Time

-- | Date on which the identity was created.
identityDescription_creationDate :: Lens.Lens' IdentityDescription (Core.Maybe Core.UTCTime)
identityDescription_creationDate = Lens.lens (\IdentityDescription' {creationDate} -> creationDate) (\s@IdentityDescription' {} a -> s {creationDate = a} :: IdentityDescription) Core.. Lens.mapping Core._Time

-- | A unique identifier in the format REGION:GUID.
identityDescription_identityId :: Lens.Lens' IdentityDescription (Core.Maybe Core.Text)
identityDescription_identityId = Lens.lens (\IdentityDescription' {identityId} -> identityId) (\s@IdentityDescription' {} a -> s {identityId = a} :: IdentityDescription)

-- | The provider names.
identityDescription_logins :: Lens.Lens' IdentityDescription (Core.Maybe [Core.Text])
identityDescription_logins = Lens.lens (\IdentityDescription' {logins} -> logins) (\s@IdentityDescription' {} a -> s {logins = a} :: IdentityDescription) Core.. Lens.mapping Lens._Coerce

instance Core.FromJSON IdentityDescription where
  parseJSON =
    Core.withObject
      "IdentityDescription"
      ( \x ->
          IdentityDescription'
            Core.<$> (x Core..:? "LastModifiedDate")
            Core.<*> (x Core..:? "CreationDate")
            Core.<*> (x Core..:? "IdentityId")
            Core.<*> (x Core..:? "Logins" Core..!= Core.mempty)
      )

instance Core.Hashable IdentityDescription

instance Core.NFData IdentityDescription
