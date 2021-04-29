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
-- Module      : Network.AWS.CognitoIdentity.Types.IdentityDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentity.Types.IdentityDescription where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A description of the identity.
--
-- /See:/ 'newIdentityDescription' smart constructor.
data IdentityDescription = IdentityDescription'
  { -- | Date on which the identity was last modified.
    lastModifiedDate :: Prelude.Maybe Prelude.POSIX,
    -- | Date on which the identity was created.
    creationDate :: Prelude.Maybe Prelude.POSIX,
    -- | A unique identifier in the format REGION:GUID.
    identityId :: Prelude.Maybe Prelude.Text,
    -- | The provider names.
    logins :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      creationDate = Prelude.Nothing,
      identityId = Prelude.Nothing,
      logins = Prelude.Nothing
    }

-- | Date on which the identity was last modified.
identityDescription_lastModifiedDate :: Lens.Lens' IdentityDescription (Prelude.Maybe Prelude.UTCTime)
identityDescription_lastModifiedDate = Lens.lens (\IdentityDescription' {lastModifiedDate} -> lastModifiedDate) (\s@IdentityDescription' {} a -> s {lastModifiedDate = a} :: IdentityDescription) Prelude.. Lens.mapping Prelude._Time

-- | Date on which the identity was created.
identityDescription_creationDate :: Lens.Lens' IdentityDescription (Prelude.Maybe Prelude.UTCTime)
identityDescription_creationDate = Lens.lens (\IdentityDescription' {creationDate} -> creationDate) (\s@IdentityDescription' {} a -> s {creationDate = a} :: IdentityDescription) Prelude.. Lens.mapping Prelude._Time

-- | A unique identifier in the format REGION:GUID.
identityDescription_identityId :: Lens.Lens' IdentityDescription (Prelude.Maybe Prelude.Text)
identityDescription_identityId = Lens.lens (\IdentityDescription' {identityId} -> identityId) (\s@IdentityDescription' {} a -> s {identityId = a} :: IdentityDescription)

-- | The provider names.
identityDescription_logins :: Lens.Lens' IdentityDescription (Prelude.Maybe [Prelude.Text])
identityDescription_logins = Lens.lens (\IdentityDescription' {logins} -> logins) (\s@IdentityDescription' {} a -> s {logins = a} :: IdentityDescription) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromJSON IdentityDescription where
  parseJSON =
    Prelude.withObject
      "IdentityDescription"
      ( \x ->
          IdentityDescription'
            Prelude.<$> (x Prelude..:? "LastModifiedDate")
            Prelude.<*> (x Prelude..:? "CreationDate")
            Prelude.<*> (x Prelude..:? "IdentityId")
            Prelude.<*> (x Prelude..:? "Logins" Prelude..!= Prelude.mempty)
      )

instance Prelude.Hashable IdentityDescription

instance Prelude.NFData IdentityDescription
