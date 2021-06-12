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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Configuration information of the security key.
--
-- /See:/ 'newSecurityKey' smart constructor.
data SecurityKey = SecurityKey'
  { -- | The key of the security key.
    key :: Core.Maybe Core.Text,
    -- | When the security key was created.
    creationTime :: Core.Maybe Core.POSIX,
    -- | The existing association identifier that uniquely identifies the
    -- resource type and storage config for the given instance ID.
    associationId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { key = Core.Nothing,
      creationTime = Core.Nothing,
      associationId = Core.Nothing
    }

-- | The key of the security key.
securityKey_key :: Lens.Lens' SecurityKey (Core.Maybe Core.Text)
securityKey_key = Lens.lens (\SecurityKey' {key} -> key) (\s@SecurityKey' {} a -> s {key = a} :: SecurityKey)

-- | When the security key was created.
securityKey_creationTime :: Lens.Lens' SecurityKey (Core.Maybe Core.UTCTime)
securityKey_creationTime = Lens.lens (\SecurityKey' {creationTime} -> creationTime) (\s@SecurityKey' {} a -> s {creationTime = a} :: SecurityKey) Core.. Lens.mapping Core._Time

-- | The existing association identifier that uniquely identifies the
-- resource type and storage config for the given instance ID.
securityKey_associationId :: Lens.Lens' SecurityKey (Core.Maybe Core.Text)
securityKey_associationId = Lens.lens (\SecurityKey' {associationId} -> associationId) (\s@SecurityKey' {} a -> s {associationId = a} :: SecurityKey)

instance Core.FromJSON SecurityKey where
  parseJSON =
    Core.withObject
      "SecurityKey"
      ( \x ->
          SecurityKey'
            Core.<$> (x Core..:? "Key")
            Core.<*> (x Core..:? "CreationTime")
            Core.<*> (x Core..:? "AssociationId")
      )

instance Core.Hashable SecurityKey

instance Core.NFData SecurityKey
