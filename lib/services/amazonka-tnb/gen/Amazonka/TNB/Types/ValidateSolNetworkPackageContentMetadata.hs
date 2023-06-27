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
-- Module      : Amazonka.TNB.Types.ValidateSolNetworkPackageContentMetadata
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.TNB.Types.ValidateSolNetworkPackageContentMetadata where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.TNB.Types.NetworkArtifactMeta

-- | Validates network package content metadata.
--
-- A network package is a .zip file in CSAR (Cloud Service Archive) format
-- defines the function packages you want to deploy and the Amazon Web
-- Services infrastructure you want to deploy them on.
--
-- /See:/ 'newValidateSolNetworkPackageContentMetadata' smart constructor.
data ValidateSolNetworkPackageContentMetadata = ValidateSolNetworkPackageContentMetadata'
  { nsd :: Prelude.Maybe NetworkArtifactMeta
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ValidateSolNetworkPackageContentMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nsd', 'validateSolNetworkPackageContentMetadata_nsd' - Undocumented member.
newValidateSolNetworkPackageContentMetadata ::
  ValidateSolNetworkPackageContentMetadata
newValidateSolNetworkPackageContentMetadata =
  ValidateSolNetworkPackageContentMetadata'
    { nsd =
        Prelude.Nothing
    }

-- | Undocumented member.
validateSolNetworkPackageContentMetadata_nsd :: Lens.Lens' ValidateSolNetworkPackageContentMetadata (Prelude.Maybe NetworkArtifactMeta)
validateSolNetworkPackageContentMetadata_nsd = Lens.lens (\ValidateSolNetworkPackageContentMetadata' {nsd} -> nsd) (\s@ValidateSolNetworkPackageContentMetadata' {} a -> s {nsd = a} :: ValidateSolNetworkPackageContentMetadata)

instance
  Data.FromJSON
    ValidateSolNetworkPackageContentMetadata
  where
  parseJSON =
    Data.withObject
      "ValidateSolNetworkPackageContentMetadata"
      ( \x ->
          ValidateSolNetworkPackageContentMetadata'
            Prelude.<$> (x Data..:? "nsd")
      )

instance
  Prelude.Hashable
    ValidateSolNetworkPackageContentMetadata
  where
  hashWithSalt
    _salt
    ValidateSolNetworkPackageContentMetadata' {..} =
      _salt `Prelude.hashWithSalt` nsd

instance
  Prelude.NFData
    ValidateSolNetworkPackageContentMetadata
  where
  rnf ValidateSolNetworkPackageContentMetadata' {..} =
    Prelude.rnf nsd
