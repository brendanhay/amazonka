{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Detective.UpdateDatasourcePackages
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a data source packages for the behavior graph.
module Amazonka.Detective.UpdateDatasourcePackages
  ( -- * Creating a Request
    UpdateDatasourcePackages (..),
    newUpdateDatasourcePackages,

    -- * Request Lenses
    updateDatasourcePackages_graphArn,
    updateDatasourcePackages_datasourcePackages,

    -- * Destructuring the Response
    UpdateDatasourcePackagesResponse (..),
    newUpdateDatasourcePackagesResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Detective.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateDatasourcePackages' smart constructor.
data UpdateDatasourcePackages = UpdateDatasourcePackages'
  { -- | The ARN of the behavior graph.
    graphArn :: Prelude.Text,
    -- | The data source package start for the behavior graph.
    datasourcePackages :: Prelude.NonEmpty DatasourcePackage
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateDatasourcePackages' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'graphArn', 'updateDatasourcePackages_graphArn' - The ARN of the behavior graph.
--
-- 'datasourcePackages', 'updateDatasourcePackages_datasourcePackages' - The data source package start for the behavior graph.
newUpdateDatasourcePackages ::
  -- | 'graphArn'
  Prelude.Text ->
  -- | 'datasourcePackages'
  Prelude.NonEmpty DatasourcePackage ->
  UpdateDatasourcePackages
newUpdateDatasourcePackages
  pGraphArn_
  pDatasourcePackages_ =
    UpdateDatasourcePackages'
      { graphArn = pGraphArn_,
        datasourcePackages =
          Lens.coerced Lens.# pDatasourcePackages_
      }

-- | The ARN of the behavior graph.
updateDatasourcePackages_graphArn :: Lens.Lens' UpdateDatasourcePackages Prelude.Text
updateDatasourcePackages_graphArn = Lens.lens (\UpdateDatasourcePackages' {graphArn} -> graphArn) (\s@UpdateDatasourcePackages' {} a -> s {graphArn = a} :: UpdateDatasourcePackages)

-- | The data source package start for the behavior graph.
updateDatasourcePackages_datasourcePackages :: Lens.Lens' UpdateDatasourcePackages (Prelude.NonEmpty DatasourcePackage)
updateDatasourcePackages_datasourcePackages = Lens.lens (\UpdateDatasourcePackages' {datasourcePackages} -> datasourcePackages) (\s@UpdateDatasourcePackages' {} a -> s {datasourcePackages = a} :: UpdateDatasourcePackages) Prelude.. Lens.coerced

instance Core.AWSRequest UpdateDatasourcePackages where
  type
    AWSResponse UpdateDatasourcePackages =
      UpdateDatasourcePackagesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull
      UpdateDatasourcePackagesResponse'

instance Prelude.Hashable UpdateDatasourcePackages where
  hashWithSalt _salt UpdateDatasourcePackages' {..} =
    _salt `Prelude.hashWithSalt` graphArn
      `Prelude.hashWithSalt` datasourcePackages

instance Prelude.NFData UpdateDatasourcePackages where
  rnf UpdateDatasourcePackages' {..} =
    Prelude.rnf graphArn
      `Prelude.seq` Prelude.rnf datasourcePackages

instance Core.ToHeaders UpdateDatasourcePackages where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateDatasourcePackages where
  toJSON UpdateDatasourcePackages' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("GraphArn" Core..= graphArn),
            Prelude.Just
              ("DatasourcePackages" Core..= datasourcePackages)
          ]
      )

instance Core.ToPath UpdateDatasourcePackages where
  toPath = Prelude.const "/graph/datasources/update"

instance Core.ToQuery UpdateDatasourcePackages where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateDatasourcePackagesResponse' smart constructor.
data UpdateDatasourcePackagesResponse = UpdateDatasourcePackagesResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateDatasourcePackagesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUpdateDatasourcePackagesResponse ::
  UpdateDatasourcePackagesResponse
newUpdateDatasourcePackagesResponse =
  UpdateDatasourcePackagesResponse'

instance
  Prelude.NFData
    UpdateDatasourcePackagesResponse
  where
  rnf _ = ()
