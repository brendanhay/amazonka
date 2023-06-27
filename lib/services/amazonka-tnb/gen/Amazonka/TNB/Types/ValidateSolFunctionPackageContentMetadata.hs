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
-- Module      : Amazonka.TNB.Types.ValidateSolFunctionPackageContentMetadata
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.TNB.Types.ValidateSolFunctionPackageContentMetadata where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.TNB.Types.FunctionArtifactMeta

-- | Validates function package content metadata.
--
-- A function package is a .zip file in CSAR (Cloud Service Archive) format
-- that contains a network function (an ETSI standard telecommunication
-- application) and function package descriptor that uses the TOSCA
-- standard to describe how the network functions should run on your
-- network.
--
-- /See:/ 'newValidateSolFunctionPackageContentMetadata' smart constructor.
data ValidateSolFunctionPackageContentMetadata = ValidateSolFunctionPackageContentMetadata'
  { vnfd :: Prelude.Maybe FunctionArtifactMeta
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ValidateSolFunctionPackageContentMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vnfd', 'validateSolFunctionPackageContentMetadata_vnfd' - Undocumented member.
newValidateSolFunctionPackageContentMetadata ::
  ValidateSolFunctionPackageContentMetadata
newValidateSolFunctionPackageContentMetadata =
  ValidateSolFunctionPackageContentMetadata'
    { vnfd =
        Prelude.Nothing
    }

-- | Undocumented member.
validateSolFunctionPackageContentMetadata_vnfd :: Lens.Lens' ValidateSolFunctionPackageContentMetadata (Prelude.Maybe FunctionArtifactMeta)
validateSolFunctionPackageContentMetadata_vnfd = Lens.lens (\ValidateSolFunctionPackageContentMetadata' {vnfd} -> vnfd) (\s@ValidateSolFunctionPackageContentMetadata' {} a -> s {vnfd = a} :: ValidateSolFunctionPackageContentMetadata)

instance
  Data.FromJSON
    ValidateSolFunctionPackageContentMetadata
  where
  parseJSON =
    Data.withObject
      "ValidateSolFunctionPackageContentMetadata"
      ( \x ->
          ValidateSolFunctionPackageContentMetadata'
            Prelude.<$> (x Data..:? "vnfd")
      )

instance
  Prelude.Hashable
    ValidateSolFunctionPackageContentMetadata
  where
  hashWithSalt
    _salt
    ValidateSolFunctionPackageContentMetadata' {..} =
      _salt `Prelude.hashWithSalt` vnfd

instance
  Prelude.NFData
    ValidateSolFunctionPackageContentMetadata
  where
  rnf ValidateSolFunctionPackageContentMetadata' {..} =
    Prelude.rnf vnfd
