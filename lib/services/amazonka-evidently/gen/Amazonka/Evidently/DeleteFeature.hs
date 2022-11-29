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
-- Module      : Amazonka.Evidently.DeleteFeature
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an Evidently feature.
module Amazonka.Evidently.DeleteFeature
  ( -- * Creating a Request
    DeleteFeature (..),
    newDeleteFeature,

    -- * Request Lenses
    deleteFeature_feature,
    deleteFeature_project,

    -- * Destructuring the Response
    DeleteFeatureResponse (..),
    newDeleteFeatureResponse,

    -- * Response Lenses
    deleteFeatureResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Evidently.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteFeature' smart constructor.
data DeleteFeature = DeleteFeature'
  { -- | The name of the feature to delete.
    feature :: Prelude.Text,
    -- | The name or ARN of the project that contains the feature to delete.
    project :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteFeature' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'feature', 'deleteFeature_feature' - The name of the feature to delete.
--
-- 'project', 'deleteFeature_project' - The name or ARN of the project that contains the feature to delete.
newDeleteFeature ::
  -- | 'feature'
  Prelude.Text ->
  -- | 'project'
  Prelude.Text ->
  DeleteFeature
newDeleteFeature pFeature_ pProject_ =
  DeleteFeature'
    { feature = pFeature_,
      project = pProject_
    }

-- | The name of the feature to delete.
deleteFeature_feature :: Lens.Lens' DeleteFeature Prelude.Text
deleteFeature_feature = Lens.lens (\DeleteFeature' {feature} -> feature) (\s@DeleteFeature' {} a -> s {feature = a} :: DeleteFeature)

-- | The name or ARN of the project that contains the feature to delete.
deleteFeature_project :: Lens.Lens' DeleteFeature Prelude.Text
deleteFeature_project = Lens.lens (\DeleteFeature' {project} -> project) (\s@DeleteFeature' {} a -> s {project = a} :: DeleteFeature)

instance Core.AWSRequest DeleteFeature where
  type
    AWSResponse DeleteFeature =
      DeleteFeatureResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteFeatureResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteFeature where
  hashWithSalt _salt DeleteFeature' {..} =
    _salt `Prelude.hashWithSalt` feature
      `Prelude.hashWithSalt` project

instance Prelude.NFData DeleteFeature where
  rnf DeleteFeature' {..} =
    Prelude.rnf feature
      `Prelude.seq` Prelude.rnf project

instance Core.ToHeaders DeleteFeature where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DeleteFeature where
  toPath DeleteFeature' {..} =
    Prelude.mconcat
      [ "/projects/",
        Core.toBS project,
        "/features/",
        Core.toBS feature
      ]

instance Core.ToQuery DeleteFeature where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteFeatureResponse' smart constructor.
data DeleteFeatureResponse = DeleteFeatureResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteFeatureResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteFeatureResponse_httpStatus' - The response's http status code.
newDeleteFeatureResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteFeatureResponse
newDeleteFeatureResponse pHttpStatus_ =
  DeleteFeatureResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteFeatureResponse_httpStatus :: Lens.Lens' DeleteFeatureResponse Prelude.Int
deleteFeatureResponse_httpStatus = Lens.lens (\DeleteFeatureResponse' {httpStatus} -> httpStatus) (\s@DeleteFeatureResponse' {} a -> s {httpStatus = a} :: DeleteFeatureResponse)

instance Prelude.NFData DeleteFeatureResponse where
  rnf DeleteFeatureResponse' {..} =
    Prelude.rnf httpStatus
