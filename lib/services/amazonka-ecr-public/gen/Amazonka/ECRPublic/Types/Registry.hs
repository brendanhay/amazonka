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
-- Module      : Amazonka.ECRPublic.Types.Registry
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ECRPublic.Types.Registry where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ECRPublic.Types.RegistryAlias
import qualified Amazonka.Prelude as Prelude

-- | The details of a public registry.
--
-- /See:/ 'newRegistry' smart constructor.
data Registry = Registry'
  { -- | The AWS account ID associated with the registry. If you do not specify a
    -- registry, the default public registry is assumed.
    registryId :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the public registry.
    registryArn :: Prelude.Text,
    -- | The URI of a public registry. The URI contains a universal prefix and
    -- the registry alias.
    registryUri :: Prelude.Text,
    -- | Whether the account is verified. This indicates whether the account is
    -- an AWS Marketplace vendor. If an account is verified, each public
    -- repository will received a verified account badge on the Amazon ECR
    -- Public Gallery.
    verified :: Prelude.Bool,
    -- | An array of objects representing the aliases for a public registry.
    aliases :: [RegistryAlias]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Registry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'registryId', 'registry_registryId' - The AWS account ID associated with the registry. If you do not specify a
-- registry, the default public registry is assumed.
--
-- 'registryArn', 'registry_registryArn' - The Amazon Resource Name (ARN) of the public registry.
--
-- 'registryUri', 'registry_registryUri' - The URI of a public registry. The URI contains a universal prefix and
-- the registry alias.
--
-- 'verified', 'registry_verified' - Whether the account is verified. This indicates whether the account is
-- an AWS Marketplace vendor. If an account is verified, each public
-- repository will received a verified account badge on the Amazon ECR
-- Public Gallery.
--
-- 'aliases', 'registry_aliases' - An array of objects representing the aliases for a public registry.
newRegistry ::
  -- | 'registryId'
  Prelude.Text ->
  -- | 'registryArn'
  Prelude.Text ->
  -- | 'registryUri'
  Prelude.Text ->
  -- | 'verified'
  Prelude.Bool ->
  Registry
newRegistry
  pRegistryId_
  pRegistryArn_
  pRegistryUri_
  pVerified_ =
    Registry'
      { registryId = pRegistryId_,
        registryArn = pRegistryArn_,
        registryUri = pRegistryUri_,
        verified = pVerified_,
        aliases = Prelude.mempty
      }

-- | The AWS account ID associated with the registry. If you do not specify a
-- registry, the default public registry is assumed.
registry_registryId :: Lens.Lens' Registry Prelude.Text
registry_registryId = Lens.lens (\Registry' {registryId} -> registryId) (\s@Registry' {} a -> s {registryId = a} :: Registry)

-- | The Amazon Resource Name (ARN) of the public registry.
registry_registryArn :: Lens.Lens' Registry Prelude.Text
registry_registryArn = Lens.lens (\Registry' {registryArn} -> registryArn) (\s@Registry' {} a -> s {registryArn = a} :: Registry)

-- | The URI of a public registry. The URI contains a universal prefix and
-- the registry alias.
registry_registryUri :: Lens.Lens' Registry Prelude.Text
registry_registryUri = Lens.lens (\Registry' {registryUri} -> registryUri) (\s@Registry' {} a -> s {registryUri = a} :: Registry)

-- | Whether the account is verified. This indicates whether the account is
-- an AWS Marketplace vendor. If an account is verified, each public
-- repository will received a verified account badge on the Amazon ECR
-- Public Gallery.
registry_verified :: Lens.Lens' Registry Prelude.Bool
registry_verified = Lens.lens (\Registry' {verified} -> verified) (\s@Registry' {} a -> s {verified = a} :: Registry)

-- | An array of objects representing the aliases for a public registry.
registry_aliases :: Lens.Lens' Registry [RegistryAlias]
registry_aliases = Lens.lens (\Registry' {aliases} -> aliases) (\s@Registry' {} a -> s {aliases = a} :: Registry) Prelude.. Lens.coerced

instance Data.FromJSON Registry where
  parseJSON =
    Data.withObject
      "Registry"
      ( \x ->
          Registry'
            Prelude.<$> (x Data..: "registryId")
            Prelude.<*> (x Data..: "registryArn")
            Prelude.<*> (x Data..: "registryUri")
            Prelude.<*> (x Data..: "verified")
            Prelude.<*> (x Data..:? "aliases" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable Registry where
  hashWithSalt _salt Registry' {..} =
    _salt
      `Prelude.hashWithSalt` registryId
      `Prelude.hashWithSalt` registryArn
      `Prelude.hashWithSalt` registryUri
      `Prelude.hashWithSalt` verified
      `Prelude.hashWithSalt` aliases

instance Prelude.NFData Registry where
  rnf Registry' {..} =
    Prelude.rnf registryId
      `Prelude.seq` Prelude.rnf registryArn
      `Prelude.seq` Prelude.rnf registryUri
      `Prelude.seq` Prelude.rnf verified
      `Prelude.seq` Prelude.rnf aliases
