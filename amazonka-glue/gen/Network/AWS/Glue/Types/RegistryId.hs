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
-- Module      : Network.AWS.Glue.Types.RegistryId
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.RegistryId where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A wrapper structure that may contain the registry name and Amazon
-- Resource Name (ARN).
--
-- /See:/ 'newRegistryId' smart constructor.
data RegistryId = RegistryId'
  { -- | Name of the registry. Used only for lookup. One of @RegistryArn@ or
    -- @RegistryName@ has to be provided.
    registryName :: Core.Maybe Core.Text,
    -- | Arn of the registry to be updated. One of @RegistryArn@ or
    -- @RegistryName@ has to be provided.
    registryArn :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RegistryId' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'registryName', 'registryId_registryName' - Name of the registry. Used only for lookup. One of @RegistryArn@ or
-- @RegistryName@ has to be provided.
--
-- 'registryArn', 'registryId_registryArn' - Arn of the registry to be updated. One of @RegistryArn@ or
-- @RegistryName@ has to be provided.
newRegistryId ::
  RegistryId
newRegistryId =
  RegistryId'
    { registryName = Core.Nothing,
      registryArn = Core.Nothing
    }

-- | Name of the registry. Used only for lookup. One of @RegistryArn@ or
-- @RegistryName@ has to be provided.
registryId_registryName :: Lens.Lens' RegistryId (Core.Maybe Core.Text)
registryId_registryName = Lens.lens (\RegistryId' {registryName} -> registryName) (\s@RegistryId' {} a -> s {registryName = a} :: RegistryId)

-- | Arn of the registry to be updated. One of @RegistryArn@ or
-- @RegistryName@ has to be provided.
registryId_registryArn :: Lens.Lens' RegistryId (Core.Maybe Core.Text)
registryId_registryArn = Lens.lens (\RegistryId' {registryArn} -> registryArn) (\s@RegistryId' {} a -> s {registryArn = a} :: RegistryId)

instance Core.Hashable RegistryId

instance Core.NFData RegistryId

instance Core.ToJSON RegistryId where
  toJSON RegistryId' {..} =
    Core.object
      ( Core.catMaybes
          [ ("RegistryName" Core..=) Core.<$> registryName,
            ("RegistryArn" Core..=) Core.<$> registryArn
          ]
      )
