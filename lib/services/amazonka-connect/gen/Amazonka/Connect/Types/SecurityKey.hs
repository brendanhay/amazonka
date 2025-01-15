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
-- Module      : Amazonka.Connect.Types.SecurityKey
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.SecurityKey where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Configuration information of the security key.
--
-- /See:/ 'newSecurityKey' smart constructor.
data SecurityKey = SecurityKey'
  { -- | The existing association identifier that uniquely identifies the
    -- resource type and storage config for the given instance ID.
    associationId :: Prelude.Maybe Prelude.Text,
    -- | When the security key was created.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | The key of the security key.
    key :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SecurityKey' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'associationId', 'securityKey_associationId' - The existing association identifier that uniquely identifies the
-- resource type and storage config for the given instance ID.
--
-- 'creationTime', 'securityKey_creationTime' - When the security key was created.
--
-- 'key', 'securityKey_key' - The key of the security key.
newSecurityKey ::
  SecurityKey
newSecurityKey =
  SecurityKey'
    { associationId = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      key = Prelude.Nothing
    }

-- | The existing association identifier that uniquely identifies the
-- resource type and storage config for the given instance ID.
securityKey_associationId :: Lens.Lens' SecurityKey (Prelude.Maybe Prelude.Text)
securityKey_associationId = Lens.lens (\SecurityKey' {associationId} -> associationId) (\s@SecurityKey' {} a -> s {associationId = a} :: SecurityKey)

-- | When the security key was created.
securityKey_creationTime :: Lens.Lens' SecurityKey (Prelude.Maybe Prelude.UTCTime)
securityKey_creationTime = Lens.lens (\SecurityKey' {creationTime} -> creationTime) (\s@SecurityKey' {} a -> s {creationTime = a} :: SecurityKey) Prelude.. Lens.mapping Data._Time

-- | The key of the security key.
securityKey_key :: Lens.Lens' SecurityKey (Prelude.Maybe Prelude.Text)
securityKey_key = Lens.lens (\SecurityKey' {key} -> key) (\s@SecurityKey' {} a -> s {key = a} :: SecurityKey)

instance Data.FromJSON SecurityKey where
  parseJSON =
    Data.withObject
      "SecurityKey"
      ( \x ->
          SecurityKey'
            Prelude.<$> (x Data..:? "AssociationId")
            Prelude.<*> (x Data..:? "CreationTime")
            Prelude.<*> (x Data..:? "Key")
      )

instance Prelude.Hashable SecurityKey where
  hashWithSalt _salt SecurityKey' {..} =
    _salt
      `Prelude.hashWithSalt` associationId
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` key

instance Prelude.NFData SecurityKey where
  rnf SecurityKey' {..} =
    Prelude.rnf associationId `Prelude.seq`
      Prelude.rnf creationTime `Prelude.seq`
        Prelude.rnf key
