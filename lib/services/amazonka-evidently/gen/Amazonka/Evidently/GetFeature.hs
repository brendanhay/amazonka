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
-- Module      : Amazonka.Evidently.GetFeature
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the details about one feature. You must already know the feature
-- name. To retrieve a list of features in your account, use
-- <https://docs.aws.amazon.com/cloudwatchevidently/latest/APIReference/API_ListFeatures.html ListFeatures>.
module Amazonka.Evidently.GetFeature
  ( -- * Creating a Request
    GetFeature (..),
    newGetFeature,

    -- * Request Lenses
    getFeature_feature,
    getFeature_project,

    -- * Destructuring the Response
    GetFeatureResponse (..),
    newGetFeatureResponse,

    -- * Response Lenses
    getFeatureResponse_httpStatus,
    getFeatureResponse_feature,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Evidently.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetFeature' smart constructor.
data GetFeature = GetFeature'
  { -- | The name of the feature that you want to retrieve information for.
    feature :: Prelude.Text,
    -- | The name or ARN of the project that contains the feature.
    project :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetFeature' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'feature', 'getFeature_feature' - The name of the feature that you want to retrieve information for.
--
-- 'project', 'getFeature_project' - The name or ARN of the project that contains the feature.
newGetFeature ::
  -- | 'feature'
  Prelude.Text ->
  -- | 'project'
  Prelude.Text ->
  GetFeature
newGetFeature pFeature_ pProject_ =
  GetFeature'
    { feature = pFeature_,
      project = pProject_
    }

-- | The name of the feature that you want to retrieve information for.
getFeature_feature :: Lens.Lens' GetFeature Prelude.Text
getFeature_feature = Lens.lens (\GetFeature' {feature} -> feature) (\s@GetFeature' {} a -> s {feature = a} :: GetFeature)

-- | The name or ARN of the project that contains the feature.
getFeature_project :: Lens.Lens' GetFeature Prelude.Text
getFeature_project = Lens.lens (\GetFeature' {project} -> project) (\s@GetFeature' {} a -> s {project = a} :: GetFeature)

instance Core.AWSRequest GetFeature where
  type AWSResponse GetFeature = GetFeatureResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetFeatureResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "feature")
      )

instance Prelude.Hashable GetFeature where
  hashWithSalt _salt GetFeature' {..} =
    _salt
      `Prelude.hashWithSalt` feature
      `Prelude.hashWithSalt` project

instance Prelude.NFData GetFeature where
  rnf GetFeature' {..} =
    Prelude.rnf feature
      `Prelude.seq` Prelude.rnf project

instance Data.ToHeaders GetFeature where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetFeature where
  toPath GetFeature' {..} =
    Prelude.mconcat
      [ "/projects/",
        Data.toBS project,
        "/features/",
        Data.toBS feature
      ]

instance Data.ToQuery GetFeature where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetFeatureResponse' smart constructor.
data GetFeatureResponse = GetFeatureResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A structure containing the configuration details of the feature.
    feature :: Feature
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetFeatureResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getFeatureResponse_httpStatus' - The response's http status code.
--
-- 'feature', 'getFeatureResponse_feature' - A structure containing the configuration details of the feature.
newGetFeatureResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'feature'
  Feature ->
  GetFeatureResponse
newGetFeatureResponse pHttpStatus_ pFeature_ =
  GetFeatureResponse'
    { httpStatus = pHttpStatus_,
      feature = pFeature_
    }

-- | The response's http status code.
getFeatureResponse_httpStatus :: Lens.Lens' GetFeatureResponse Prelude.Int
getFeatureResponse_httpStatus = Lens.lens (\GetFeatureResponse' {httpStatus} -> httpStatus) (\s@GetFeatureResponse' {} a -> s {httpStatus = a} :: GetFeatureResponse)

-- | A structure containing the configuration details of the feature.
getFeatureResponse_feature :: Lens.Lens' GetFeatureResponse Feature
getFeatureResponse_feature = Lens.lens (\GetFeatureResponse' {feature} -> feature) (\s@GetFeatureResponse' {} a -> s {feature = a} :: GetFeatureResponse)

instance Prelude.NFData GetFeatureResponse where
  rnf GetFeatureResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf feature
