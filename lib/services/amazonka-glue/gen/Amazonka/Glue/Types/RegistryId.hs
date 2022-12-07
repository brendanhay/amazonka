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
-- Module      : Amazonka.Glue.Types.RegistryId
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.RegistryId where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Prelude.Hashable RegistryId where
  hashWithSalt _salt RegistryId' {..} =
    _salt `Prelude.hashWithSalt` registryName
      `Prelude.hashWithSalt` registryArn

instance Prelude.NFData RegistryId where
  rnf RegistryId' {..} =
    Prelude.rnf registryName
      `Prelude.seq` Prelude.rnf registryArn

instance Data.ToJSON RegistryId where
  toJSON RegistryId' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("RegistryName" Data..=) Prelude.<$> registryName,
            ("RegistryArn" Data..=) Prelude.<$> registryArn
          ]
      )
