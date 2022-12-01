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
-- Module      : Amazonka.LakeFormation.Types.DataLakePrincipal
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LakeFormation.Types.DataLakePrincipal where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The Lake Formation principal. Supported principals are IAM users or IAM
-- roles.
--
-- /See:/ 'newDataLakePrincipal' smart constructor.
data DataLakePrincipal = DataLakePrincipal'
  { -- | An identifier for the Lake Formation principal.
    dataLakePrincipalIdentifier :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DataLakePrincipal' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataLakePrincipalIdentifier', 'dataLakePrincipal_dataLakePrincipalIdentifier' - An identifier for the Lake Formation principal.
newDataLakePrincipal ::
  DataLakePrincipal
newDataLakePrincipal =
  DataLakePrincipal'
    { dataLakePrincipalIdentifier =
        Prelude.Nothing
    }

-- | An identifier for the Lake Formation principal.
dataLakePrincipal_dataLakePrincipalIdentifier :: Lens.Lens' DataLakePrincipal (Prelude.Maybe Prelude.Text)
dataLakePrincipal_dataLakePrincipalIdentifier = Lens.lens (\DataLakePrincipal' {dataLakePrincipalIdentifier} -> dataLakePrincipalIdentifier) (\s@DataLakePrincipal' {} a -> s {dataLakePrincipalIdentifier = a} :: DataLakePrincipal)

instance Core.FromJSON DataLakePrincipal where
  parseJSON =
    Core.withObject
      "DataLakePrincipal"
      ( \x ->
          DataLakePrincipal'
            Prelude.<$> (x Core..:? "DataLakePrincipalIdentifier")
      )

instance Prelude.Hashable DataLakePrincipal where
  hashWithSalt _salt DataLakePrincipal' {..} =
    _salt
      `Prelude.hashWithSalt` dataLakePrincipalIdentifier

instance Prelude.NFData DataLakePrincipal where
  rnf DataLakePrincipal' {..} =
    Prelude.rnf dataLakePrincipalIdentifier

instance Core.ToJSON DataLakePrincipal where
  toJSON DataLakePrincipal' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("DataLakePrincipalIdentifier" Core..=)
              Prelude.<$> dataLakePrincipalIdentifier
          ]
      )
