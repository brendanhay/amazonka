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
-- Module      : Amazonka.TNB.Types.PutSolNetworkPackageContentMetadata
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.TNB.Types.PutSolNetworkPackageContentMetadata where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.TNB.Types.NetworkArtifactMeta

-- | Update metadata in a network package.
--
-- A network package is a .zip file in CSAR (Cloud Service Archive) format
-- defines the function packages you want to deploy and the Amazon Web
-- Services infrastructure you want to deploy them on.
--
-- /See:/ 'newPutSolNetworkPackageContentMetadata' smart constructor.
data PutSolNetworkPackageContentMetadata = PutSolNetworkPackageContentMetadata'
  { nsd :: Prelude.Maybe NetworkArtifactMeta
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutSolNetworkPackageContentMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nsd', 'putSolNetworkPackageContentMetadata_nsd' - Undocumented member.
newPutSolNetworkPackageContentMetadata ::
  PutSolNetworkPackageContentMetadata
newPutSolNetworkPackageContentMetadata =
  PutSolNetworkPackageContentMetadata'
    { nsd =
        Prelude.Nothing
    }

-- | Undocumented member.
putSolNetworkPackageContentMetadata_nsd :: Lens.Lens' PutSolNetworkPackageContentMetadata (Prelude.Maybe NetworkArtifactMeta)
putSolNetworkPackageContentMetadata_nsd = Lens.lens (\PutSolNetworkPackageContentMetadata' {nsd} -> nsd) (\s@PutSolNetworkPackageContentMetadata' {} a -> s {nsd = a} :: PutSolNetworkPackageContentMetadata)

instance
  Data.FromJSON
    PutSolNetworkPackageContentMetadata
  where
  parseJSON =
    Data.withObject
      "PutSolNetworkPackageContentMetadata"
      ( \x ->
          PutSolNetworkPackageContentMetadata'
            Prelude.<$> (x Data..:? "nsd")
      )

instance
  Prelude.Hashable
    PutSolNetworkPackageContentMetadata
  where
  hashWithSalt
    _salt
    PutSolNetworkPackageContentMetadata' {..} =
      _salt `Prelude.hashWithSalt` nsd

instance
  Prelude.NFData
    PutSolNetworkPackageContentMetadata
  where
  rnf PutSolNetworkPackageContentMetadata' {..} =
    Prelude.rnf nsd
