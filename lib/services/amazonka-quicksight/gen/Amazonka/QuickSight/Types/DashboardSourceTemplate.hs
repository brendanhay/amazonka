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
-- Module      : Amazonka.QuickSight.Types.DashboardSourceTemplate
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.DashboardSourceTemplate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.DataSetReference

-- | Dashboard source template.
--
-- /See:/ 'newDashboardSourceTemplate' smart constructor.
data DashboardSourceTemplate = DashboardSourceTemplate'
  { -- | Dataset references.
    dataSetReferences :: Prelude.NonEmpty DataSetReference,
    -- | The Amazon Resource Name (ARN) of the resource.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DashboardSourceTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataSetReferences', 'dashboardSourceTemplate_dataSetReferences' - Dataset references.
--
-- 'arn', 'dashboardSourceTemplate_arn' - The Amazon Resource Name (ARN) of the resource.
newDashboardSourceTemplate ::
  -- | 'dataSetReferences'
  Prelude.NonEmpty DataSetReference ->
  -- | 'arn'
  Prelude.Text ->
  DashboardSourceTemplate
newDashboardSourceTemplate pDataSetReferences_ pArn_ =
  DashboardSourceTemplate'
    { dataSetReferences =
        Lens.coerced Lens.# pDataSetReferences_,
      arn = pArn_
    }

-- | Dataset references.
dashboardSourceTemplate_dataSetReferences :: Lens.Lens' DashboardSourceTemplate (Prelude.NonEmpty DataSetReference)
dashboardSourceTemplate_dataSetReferences = Lens.lens (\DashboardSourceTemplate' {dataSetReferences} -> dataSetReferences) (\s@DashboardSourceTemplate' {} a -> s {dataSetReferences = a} :: DashboardSourceTemplate) Prelude.. Lens.coerced

-- | The Amazon Resource Name (ARN) of the resource.
dashboardSourceTemplate_arn :: Lens.Lens' DashboardSourceTemplate Prelude.Text
dashboardSourceTemplate_arn = Lens.lens (\DashboardSourceTemplate' {arn} -> arn) (\s@DashboardSourceTemplate' {} a -> s {arn = a} :: DashboardSourceTemplate)

instance Prelude.Hashable DashboardSourceTemplate where
  hashWithSalt _salt DashboardSourceTemplate' {..} =
    _salt `Prelude.hashWithSalt` dataSetReferences
      `Prelude.hashWithSalt` arn

instance Prelude.NFData DashboardSourceTemplate where
  rnf DashboardSourceTemplate' {..} =
    Prelude.rnf dataSetReferences
      `Prelude.seq` Prelude.rnf arn

instance Data.ToJSON DashboardSourceTemplate where
  toJSON DashboardSourceTemplate' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("DataSetReferences" Data..= dataSetReferences),
            Prelude.Just ("Arn" Data..= arn)
          ]
      )
