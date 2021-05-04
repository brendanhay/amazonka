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
-- Module      : Network.AWS.Connect.Types.SecurityKey
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.SecurityKey where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Configuration information of the security key.
--
-- /See:/ 'newSecurityKey' smart constructor.
data SecurityKey = SecurityKey'
  { -- | The key of the security key.
    key :: Prelude.Maybe Prelude.Text,
    -- | When the security key was created.
    creationTime :: Prelude.Maybe Prelude.POSIX,
    -- | The existing association identifier that uniquely identifies the
    -- resource type and storage config for the given instance ID.
    associationId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
securityKey_creationTime = Lens.lens (\SecurityKey' {creationTime} -> creationTime) (\s@SecurityKey' {} a -> s {creationTime = a} :: SecurityKey) Prelude.. Lens.mapping Prelude._Time

-- | The existing association identifier that uniquely identifies the
-- resource type and storage config for the given instance ID.
securityKey_associationId :: Lens.Lens' SecurityKey (Prelude.Maybe Prelude.Text)
securityKey_associationId = Lens.lens (\SecurityKey' {associationId} -> associationId) (\s@SecurityKey' {} a -> s {associationId = a} :: SecurityKey)

instance Prelude.FromJSON SecurityKey where
  parseJSON =
    Prelude.withObject
      "SecurityKey"
      ( \x ->
          SecurityKey'
            Prelude.<$> (x Prelude..:? "Key")
            Prelude.<*> (x Prelude..:? "CreationTime")
            Prelude.<*> (x Prelude..:? "AssociationId")
      )

instance Prelude.Hashable SecurityKey

instance Prelude.NFData SecurityKey
