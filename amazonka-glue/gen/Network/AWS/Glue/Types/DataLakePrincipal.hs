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
-- Module      : Network.AWS.Glue.Types.DataLakePrincipal
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.DataLakePrincipal where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The AWS Lake Formation principal.
--
-- /See:/ 'newDataLakePrincipal' smart constructor.
data DataLakePrincipal = DataLakePrincipal'
  { -- | An identifier for the AWS Lake Formation principal.
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
-- 'dataLakePrincipalIdentifier', 'dataLakePrincipal_dataLakePrincipalIdentifier' - An identifier for the AWS Lake Formation principal.
newDataLakePrincipal ::
  DataLakePrincipal
newDataLakePrincipal =
  DataLakePrincipal'
    { dataLakePrincipalIdentifier =
        Prelude.Nothing
    }

-- | An identifier for the AWS Lake Formation principal.
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

instance Prelude.Hashable DataLakePrincipal

instance Prelude.NFData DataLakePrincipal

instance Core.ToJSON DataLakePrincipal where
  toJSON DataLakePrincipal' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("DataLakePrincipalIdentifier" Core..=)
              Prelude.<$> dataLakePrincipalIdentifier
          ]
      )
