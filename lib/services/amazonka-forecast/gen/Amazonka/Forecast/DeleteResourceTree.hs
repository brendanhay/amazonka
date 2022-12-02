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
-- Module      : Amazonka.Forecast.DeleteResourceTree
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an entire resource tree. This operation will delete the parent
-- resource and its child resources.
--
-- Child resources are resources that were created from another resource.
-- For example, when a forecast is generated from a predictor, the forecast
-- is the child resource and the predictor is the parent resource.
--
-- Amazon Forecast resources possess the following parent-child resource
-- hierarchies:
--
-- -   __Dataset__: dataset import jobs
--
-- -   __Dataset Group__: predictors, predictor backtest export jobs,
--     forecasts, forecast export jobs
--
-- -   __Predictor__: predictor backtest export jobs, forecasts, forecast
--     export jobs
--
-- -   __Forecast__: forecast export jobs
--
-- @DeleteResourceTree@ will only delete Amazon Forecast resources, and
-- will not delete datasets or exported files stored in Amazon S3.
module Amazonka.Forecast.DeleteResourceTree
  ( -- * Creating a Request
    DeleteResourceTree (..),
    newDeleteResourceTree,

    -- * Request Lenses
    deleteResourceTree_resourceArn,

    -- * Destructuring the Response
    DeleteResourceTreeResponse (..),
    newDeleteResourceTreeResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Forecast.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteResourceTree' smart constructor.
data DeleteResourceTree = DeleteResourceTree'
  { -- | The Amazon Resource Name (ARN) of the parent resource to delete. All
    -- child resources of the parent resource will also be deleted.
    resourceArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteResourceTree' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceArn', 'deleteResourceTree_resourceArn' - The Amazon Resource Name (ARN) of the parent resource to delete. All
-- child resources of the parent resource will also be deleted.
newDeleteResourceTree ::
  -- | 'resourceArn'
  Prelude.Text ->
  DeleteResourceTree
newDeleteResourceTree pResourceArn_ =
  DeleteResourceTree' {resourceArn = pResourceArn_}

-- | The Amazon Resource Name (ARN) of the parent resource to delete. All
-- child resources of the parent resource will also be deleted.
deleteResourceTree_resourceArn :: Lens.Lens' DeleteResourceTree Prelude.Text
deleteResourceTree_resourceArn = Lens.lens (\DeleteResourceTree' {resourceArn} -> resourceArn) (\s@DeleteResourceTree' {} a -> s {resourceArn = a} :: DeleteResourceTree)

instance Core.AWSRequest DeleteResourceTree where
  type
    AWSResponse DeleteResourceTree =
      DeleteResourceTreeResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull DeleteResourceTreeResponse'

instance Prelude.Hashable DeleteResourceTree where
  hashWithSalt _salt DeleteResourceTree' {..} =
    _salt `Prelude.hashWithSalt` resourceArn

instance Prelude.NFData DeleteResourceTree where
  rnf DeleteResourceTree' {..} = Prelude.rnf resourceArn

instance Data.ToHeaders DeleteResourceTree where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonForecast.DeleteResourceTree" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteResourceTree where
  toJSON DeleteResourceTree' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("ResourceArn" Data..= resourceArn)]
      )

instance Data.ToPath DeleteResourceTree where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteResourceTree where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteResourceTreeResponse' smart constructor.
data DeleteResourceTreeResponse = DeleteResourceTreeResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteResourceTreeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteResourceTreeResponse ::
  DeleteResourceTreeResponse
newDeleteResourceTreeResponse =
  DeleteResourceTreeResponse'

instance Prelude.NFData DeleteResourceTreeResponse where
  rnf _ = ()
