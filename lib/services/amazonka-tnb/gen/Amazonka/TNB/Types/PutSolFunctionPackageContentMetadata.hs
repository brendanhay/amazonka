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
-- Module      : Amazonka.TNB.Types.PutSolFunctionPackageContentMetadata
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.TNB.Types.PutSolFunctionPackageContentMetadata where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.TNB.Types.FunctionArtifactMeta

-- | Update metadata in a function package.
--
-- A function package is a .zip file in CSAR (Cloud Service Archive) format
-- that contains a network function (an ETSI standard telecommunication
-- application) and function package descriptor that uses the TOSCA
-- standard to describe how the network functions should run on your
-- network.
--
-- /See:/ 'newPutSolFunctionPackageContentMetadata' smart constructor.
data PutSolFunctionPackageContentMetadata = PutSolFunctionPackageContentMetadata'
  { vnfd :: Prelude.Maybe FunctionArtifactMeta
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutSolFunctionPackageContentMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vnfd', 'putSolFunctionPackageContentMetadata_vnfd' - Undocumented member.
newPutSolFunctionPackageContentMetadata ::
  PutSolFunctionPackageContentMetadata
newPutSolFunctionPackageContentMetadata =
  PutSolFunctionPackageContentMetadata'
    { vnfd =
        Prelude.Nothing
    }

-- | Undocumented member.
putSolFunctionPackageContentMetadata_vnfd :: Lens.Lens' PutSolFunctionPackageContentMetadata (Prelude.Maybe FunctionArtifactMeta)
putSolFunctionPackageContentMetadata_vnfd = Lens.lens (\PutSolFunctionPackageContentMetadata' {vnfd} -> vnfd) (\s@PutSolFunctionPackageContentMetadata' {} a -> s {vnfd = a} :: PutSolFunctionPackageContentMetadata)

instance
  Data.FromJSON
    PutSolFunctionPackageContentMetadata
  where
  parseJSON =
    Data.withObject
      "PutSolFunctionPackageContentMetadata"
      ( \x ->
          PutSolFunctionPackageContentMetadata'
            Prelude.<$> (x Data..:? "vnfd")
      )

instance
  Prelude.Hashable
    PutSolFunctionPackageContentMetadata
  where
  hashWithSalt
    _salt
    PutSolFunctionPackageContentMetadata' {..} =
      _salt `Prelude.hashWithSalt` vnfd

instance
  Prelude.NFData
    PutSolFunctionPackageContentMetadata
  where
  rnf PutSolFunctionPackageContentMetadata' {..} =
    Prelude.rnf vnfd
