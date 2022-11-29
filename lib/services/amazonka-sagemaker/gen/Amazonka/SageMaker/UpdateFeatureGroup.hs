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
-- Module      : Amazonka.SageMaker.UpdateFeatureGroup
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the feature group.
module Amazonka.SageMaker.UpdateFeatureGroup
  ( -- * Creating a Request
    UpdateFeatureGroup (..),
    newUpdateFeatureGroup,

    -- * Request Lenses
    updateFeatureGroup_featureAdditions,
    updateFeatureGroup_featureGroupName,

    -- * Destructuring the Response
    UpdateFeatureGroupResponse (..),
    newUpdateFeatureGroupResponse,

    -- * Response Lenses
    updateFeatureGroupResponse_httpStatus,
    updateFeatureGroupResponse_featureGroupArn,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newUpdateFeatureGroup' smart constructor.
data UpdateFeatureGroup = UpdateFeatureGroup'
  { -- | Updates the feature group. Updating a feature group is an asynchronous
    -- operation. When you get an HTTP 200 response, you\'ve made a valid
    -- request. It takes some time after you\'ve made a valid request for
    -- Feature Store to update the feature group.
    featureAdditions :: Prelude.Maybe (Prelude.NonEmpty FeatureDefinition),
    -- | The name of the feature group that you\'re updating.
    featureGroupName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateFeatureGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'featureAdditions', 'updateFeatureGroup_featureAdditions' - Updates the feature group. Updating a feature group is an asynchronous
-- operation. When you get an HTTP 200 response, you\'ve made a valid
-- request. It takes some time after you\'ve made a valid request for
-- Feature Store to update the feature group.
--
-- 'featureGroupName', 'updateFeatureGroup_featureGroupName' - The name of the feature group that you\'re updating.
newUpdateFeatureGroup ::
  -- | 'featureGroupName'
  Prelude.Text ->
  UpdateFeatureGroup
newUpdateFeatureGroup pFeatureGroupName_ =
  UpdateFeatureGroup'
    { featureAdditions =
        Prelude.Nothing,
      featureGroupName = pFeatureGroupName_
    }

-- | Updates the feature group. Updating a feature group is an asynchronous
-- operation. When you get an HTTP 200 response, you\'ve made a valid
-- request. It takes some time after you\'ve made a valid request for
-- Feature Store to update the feature group.
updateFeatureGroup_featureAdditions :: Lens.Lens' UpdateFeatureGroup (Prelude.Maybe (Prelude.NonEmpty FeatureDefinition))
updateFeatureGroup_featureAdditions = Lens.lens (\UpdateFeatureGroup' {featureAdditions} -> featureAdditions) (\s@UpdateFeatureGroup' {} a -> s {featureAdditions = a} :: UpdateFeatureGroup) Prelude.. Lens.mapping Lens.coerced

-- | The name of the feature group that you\'re updating.
updateFeatureGroup_featureGroupName :: Lens.Lens' UpdateFeatureGroup Prelude.Text
updateFeatureGroup_featureGroupName = Lens.lens (\UpdateFeatureGroup' {featureGroupName} -> featureGroupName) (\s@UpdateFeatureGroup' {} a -> s {featureGroupName = a} :: UpdateFeatureGroup)

instance Core.AWSRequest UpdateFeatureGroup where
  type
    AWSResponse UpdateFeatureGroup =
      UpdateFeatureGroupResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateFeatureGroupResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "FeatureGroupArn")
      )

instance Prelude.Hashable UpdateFeatureGroup where
  hashWithSalt _salt UpdateFeatureGroup' {..} =
    _salt `Prelude.hashWithSalt` featureAdditions
      `Prelude.hashWithSalt` featureGroupName

instance Prelude.NFData UpdateFeatureGroup where
  rnf UpdateFeatureGroup' {..} =
    Prelude.rnf featureAdditions
      `Prelude.seq` Prelude.rnf featureGroupName

instance Core.ToHeaders UpdateFeatureGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "SageMaker.UpdateFeatureGroup" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateFeatureGroup where
  toJSON UpdateFeatureGroup' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("FeatureAdditions" Core..=)
              Prelude.<$> featureAdditions,
            Prelude.Just
              ("FeatureGroupName" Core..= featureGroupName)
          ]
      )

instance Core.ToPath UpdateFeatureGroup where
  toPath = Prelude.const "/"

instance Core.ToQuery UpdateFeatureGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateFeatureGroupResponse' smart constructor.
data UpdateFeatureGroupResponse = UpdateFeatureGroupResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The Amazon Resource Number (ARN) of the feature group that you\'re
    -- updating.
    featureGroupArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateFeatureGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateFeatureGroupResponse_httpStatus' - The response's http status code.
--
-- 'featureGroupArn', 'updateFeatureGroupResponse_featureGroupArn' - The Amazon Resource Number (ARN) of the feature group that you\'re
-- updating.
newUpdateFeatureGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'featureGroupArn'
  Prelude.Text ->
  UpdateFeatureGroupResponse
newUpdateFeatureGroupResponse
  pHttpStatus_
  pFeatureGroupArn_ =
    UpdateFeatureGroupResponse'
      { httpStatus =
          pHttpStatus_,
        featureGroupArn = pFeatureGroupArn_
      }

-- | The response's http status code.
updateFeatureGroupResponse_httpStatus :: Lens.Lens' UpdateFeatureGroupResponse Prelude.Int
updateFeatureGroupResponse_httpStatus = Lens.lens (\UpdateFeatureGroupResponse' {httpStatus} -> httpStatus) (\s@UpdateFeatureGroupResponse' {} a -> s {httpStatus = a} :: UpdateFeatureGroupResponse)

-- | The Amazon Resource Number (ARN) of the feature group that you\'re
-- updating.
updateFeatureGroupResponse_featureGroupArn :: Lens.Lens' UpdateFeatureGroupResponse Prelude.Text
updateFeatureGroupResponse_featureGroupArn = Lens.lens (\UpdateFeatureGroupResponse' {featureGroupArn} -> featureGroupArn) (\s@UpdateFeatureGroupResponse' {} a -> s {featureGroupArn = a} :: UpdateFeatureGroupResponse)

instance Prelude.NFData UpdateFeatureGroupResponse where
  rnf UpdateFeatureGroupResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf featureGroupArn
