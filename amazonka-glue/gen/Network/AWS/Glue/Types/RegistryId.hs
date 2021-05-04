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
-- Module      : Network.AWS.Glue.Types.RegistryId
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.RegistryId where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A wrapper structure that may contain the registry name and Amazon
-- Resource Name (ARN).
--
-- /See:/ 'newRegistryId' smart constructor.
data RegistryId = RegistryId'
  { -- | Name of the registry. Used only for lookup. One of @RegistryArn@ or
    -- @RegistryName@ has to be provided.
    registryName :: Prelude.Maybe Prelude.Text,
    -- | Arn of the registry to be updated. One of @RegistryArn@ or
    -- @RegistryName@ has to be provided.
    registryArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { registryName = Prelude.Nothing,
      registryArn = Prelude.Nothing
    }

-- | Name of the registry. Used only for lookup. One of @RegistryArn@ or
-- @RegistryName@ has to be provided.
registryId_registryName :: Lens.Lens' RegistryId (Prelude.Maybe Prelude.Text)
registryId_registryName = Lens.lens (\RegistryId' {registryName} -> registryName) (\s@RegistryId' {} a -> s {registryName = a} :: RegistryId)

-- | Arn of the registry to be updated. One of @RegistryArn@ or
-- @RegistryName@ has to be provided.
registryId_registryArn :: Lens.Lens' RegistryId (Prelude.Maybe Prelude.Text)
registryId_registryArn = Lens.lens (\RegistryId' {registryArn} -> registryArn) (\s@RegistryId' {} a -> s {registryArn = a} :: RegistryId)

instance Prelude.Hashable RegistryId

instance Prelude.NFData RegistryId

instance Prelude.ToJSON RegistryId where
  toJSON RegistryId' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("RegistryName" Prelude..=)
              Prelude.<$> registryName,
            ("RegistryArn" Prelude..=) Prelude.<$> registryArn
          ]
      )
