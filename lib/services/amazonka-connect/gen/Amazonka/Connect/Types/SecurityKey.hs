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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
  { -- | The key of the security key.
    key :: Prelude.Maybe Prelude.Text,
    -- | When the security key was created.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | The existing association identifier that uniquely identifies the
    -- resource type and storage config for the given instance ID.
    associationId :: Prelude.Maybe Prelude.Text
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
-- 'key', 'securityKey_key' - The key of the security key.
--
-- 'creationTime', 'securityKey_creationTime' - When the security key was created.
--
-- 'associationId', 'securityKey_associationId' - The existing association identifier that uniquely identifies the
-- resource type and storage config for the given instance ID.
newSecurityKey ::
  SecurityKey
newSecurityKey =
  SecurityKey'
    { key = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      associationId = Prelude.Nothing
    }

-- | The key of the security key.
securityKey_key :: Lens.Lens' SecurityKey (Prelude.Maybe Prelude.Text)
securityKey_key = Lens.lens (\SecurityKey' {key} -> key) (\s@SecurityKey' {} a -> s {key = a} :: SecurityKey)

-- | When the security key was created.
securityKey_creationTime :: Lens.Lens' SecurityKey (Prelude.Maybe Prelude.UTCTime)
securityKey_creationTime = Lens.lens (\SecurityKey' {creationTime} -> creationTime) (\s@SecurityKey' {} a -> s {creationTime = a} :: SecurityKey) Prelude.. Lens.mapping Data._Time

-- | The existing association identifier that uniquely identifies the
-- resource type and storage config for the given instance ID.
securityKey_associationId :: Lens.Lens' SecurityKey (Prelude.Maybe Prelude.Text)
securityKey_associationId = Lens.lens (\SecurityKey' {associationId} -> associationId) (\s@SecurityKey' {} a -> s {associationId = a} :: SecurityKey)

instance Data.FromJSON SecurityKey where
  parseJSON =
    Data.withObject
      "SecurityKey"
      ( \x ->
          SecurityKey'
            Prelude.<$> (x Data..:? "Key")
            Prelude.<*> (x Data..:? "CreationTime")
            Prelude.<*> (x Data..:? "AssociationId")
      )

instance Prelude.Hashable SecurityKey where
  hashWithSalt _salt SecurityKey' {..} =
    _salt `Prelude.hashWithSalt` key
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` associationId

instance Prelude.NFData SecurityKey where
  rnf SecurityKey' {..} =
    Prelude.rnf key
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf associationId
