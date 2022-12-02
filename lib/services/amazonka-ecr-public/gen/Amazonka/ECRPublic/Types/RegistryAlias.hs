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
-- Module      : Amazonka.ECRPublic.Types.RegistryAlias
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ECRPublic.Types.RegistryAlias where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ECRPublic.Types.RegistryAliasStatus
import qualified Amazonka.Prelude as Prelude

-- | An object representing the aliases for a public registry. A public
-- registry is given an alias upon creation but a custom alias can be set
-- using the Amazon ECR console. For more information, see
-- <https://docs.aws.amazon.com/AmazonECR/latest/userguide/Registries.html Registries>
-- in the /Amazon Elastic Container Registry User Guide/.
--
-- /See:/ 'newRegistryAlias' smart constructor.
data RegistryAlias = RegistryAlias'
  { -- | The name of the registry alias.
    name :: Prelude.Text,
    -- | The status of the registry alias.
    status :: RegistryAliasStatus,
    -- | Whether or not the registry alias is the primary alias for the registry.
    -- If true, the alias is the primary registry alias and is displayed in
    -- both the repository URL and the image URI used in the @docker pull@
    -- commands on the Amazon ECR Public Gallery.
    --
    -- A registry alias that is not the primary registry alias can be used in
    -- the repository URI in a @docker pull@ command.
    primaryRegistryAlias :: Prelude.Bool,
    -- | Whether or not the registry alias is the default alias for the registry.
    -- When the first public repository is created, your public registry is
    -- assigned a default registry alias.
    defaultRegistryAlias :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RegistryAlias' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'registryAlias_name' - The name of the registry alias.
--
-- 'status', 'registryAlias_status' - The status of the registry alias.
--
-- 'primaryRegistryAlias', 'registryAlias_primaryRegistryAlias' - Whether or not the registry alias is the primary alias for the registry.
-- If true, the alias is the primary registry alias and is displayed in
-- both the repository URL and the image URI used in the @docker pull@
-- commands on the Amazon ECR Public Gallery.
--
-- A registry alias that is not the primary registry alias can be used in
-- the repository URI in a @docker pull@ command.
--
-- 'defaultRegistryAlias', 'registryAlias_defaultRegistryAlias' - Whether or not the registry alias is the default alias for the registry.
-- When the first public repository is created, your public registry is
-- assigned a default registry alias.
newRegistryAlias ::
  -- | 'name'
  Prelude.Text ->
  -- | 'status'
  RegistryAliasStatus ->
  -- | 'primaryRegistryAlias'
  Prelude.Bool ->
  -- | 'defaultRegistryAlias'
  Prelude.Bool ->
  RegistryAlias
newRegistryAlias
  pName_
  pStatus_
  pPrimaryRegistryAlias_
  pDefaultRegistryAlias_ =
    RegistryAlias'
      { name = pName_,
        status = pStatus_,
        primaryRegistryAlias = pPrimaryRegistryAlias_,
        defaultRegistryAlias = pDefaultRegistryAlias_
      }

-- | The name of the registry alias.
registryAlias_name :: Lens.Lens' RegistryAlias Prelude.Text
registryAlias_name = Lens.lens (\RegistryAlias' {name} -> name) (\s@RegistryAlias' {} a -> s {name = a} :: RegistryAlias)

-- | The status of the registry alias.
registryAlias_status :: Lens.Lens' RegistryAlias RegistryAliasStatus
registryAlias_status = Lens.lens (\RegistryAlias' {status} -> status) (\s@RegistryAlias' {} a -> s {status = a} :: RegistryAlias)

-- | Whether or not the registry alias is the primary alias for the registry.
-- If true, the alias is the primary registry alias and is displayed in
-- both the repository URL and the image URI used in the @docker pull@
-- commands on the Amazon ECR Public Gallery.
--
-- A registry alias that is not the primary registry alias can be used in
-- the repository URI in a @docker pull@ command.
registryAlias_primaryRegistryAlias :: Lens.Lens' RegistryAlias Prelude.Bool
registryAlias_primaryRegistryAlias = Lens.lens (\RegistryAlias' {primaryRegistryAlias} -> primaryRegistryAlias) (\s@RegistryAlias' {} a -> s {primaryRegistryAlias = a} :: RegistryAlias)

-- | Whether or not the registry alias is the default alias for the registry.
-- When the first public repository is created, your public registry is
-- assigned a default registry alias.
registryAlias_defaultRegistryAlias :: Lens.Lens' RegistryAlias Prelude.Bool
registryAlias_defaultRegistryAlias = Lens.lens (\RegistryAlias' {defaultRegistryAlias} -> defaultRegistryAlias) (\s@RegistryAlias' {} a -> s {defaultRegistryAlias = a} :: RegistryAlias)

instance Data.FromJSON RegistryAlias where
  parseJSON =
    Data.withObject
      "RegistryAlias"
      ( \x ->
          RegistryAlias'
            Prelude.<$> (x Data..: "name")
            Prelude.<*> (x Data..: "status")
            Prelude.<*> (x Data..: "primaryRegistryAlias")
            Prelude.<*> (x Data..: "defaultRegistryAlias")
      )

instance Prelude.Hashable RegistryAlias where
  hashWithSalt _salt RegistryAlias' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` primaryRegistryAlias
      `Prelude.hashWithSalt` defaultRegistryAlias

instance Prelude.NFData RegistryAlias where
  rnf RegistryAlias' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf primaryRegistryAlias
      `Prelude.seq` Prelude.rnf defaultRegistryAlias
