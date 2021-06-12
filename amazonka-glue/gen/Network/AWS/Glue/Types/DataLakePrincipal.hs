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

-- | The AWS Lake Formation principal.
--
-- /See:/ 'newDataLakePrincipal' smart constructor.
data DataLakePrincipal = DataLakePrincipal'
  { -- | An identifier for the AWS Lake Formation principal.
    dataLakePrincipalIdentifier :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
        Core.Nothing
    }

-- | An identifier for the AWS Lake Formation principal.
dataLakePrincipal_dataLakePrincipalIdentifier :: Lens.Lens' DataLakePrincipal (Core.Maybe Core.Text)
dataLakePrincipal_dataLakePrincipalIdentifier = Lens.lens (\DataLakePrincipal' {dataLakePrincipalIdentifier} -> dataLakePrincipalIdentifier) (\s@DataLakePrincipal' {} a -> s {dataLakePrincipalIdentifier = a} :: DataLakePrincipal)

instance Core.FromJSON DataLakePrincipal where
  parseJSON =
    Core.withObject
      "DataLakePrincipal"
      ( \x ->
          DataLakePrincipal'
            Core.<$> (x Core..:? "DataLakePrincipalIdentifier")
      )

instance Core.Hashable DataLakePrincipal

instance Core.NFData DataLakePrincipal

instance Core.ToJSON DataLakePrincipal where
  toJSON DataLakePrincipal' {..} =
    Core.object
      ( Core.catMaybes
          [ ("DataLakePrincipalIdentifier" Core..=)
              Core.<$> dataLakePrincipalIdentifier
          ]
      )
