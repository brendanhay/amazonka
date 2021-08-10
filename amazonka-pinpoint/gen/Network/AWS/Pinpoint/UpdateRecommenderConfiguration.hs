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
-- Module      : Network.AWS.Pinpoint.UpdateRecommenderConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an Amazon Pinpoint configuration for a recommender model.
module Network.AWS.Pinpoint.UpdateRecommenderConfiguration
  ( -- * Creating a Request
    UpdateRecommenderConfiguration' (..),
    newUpdateRecommenderConfiguration',

    -- * Request Lenses
    updateRecommenderConfiguration'_recommenderId,
    updateRecommenderConfiguration'_updateRecommenderConfiguration,

    -- * Destructuring the Response
    UpdateRecommenderConfigurationResponse (..),
    newUpdateRecommenderConfigurationResponse,

    -- * Response Lenses
    updateRecommenderConfigurationResponse_httpStatus,
    updateRecommenderConfigurationResponse_recommenderConfigurationResponse,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateRecommenderConfiguration'' smart constructor.
data UpdateRecommenderConfiguration' = UpdateRecommenderConfiguration''
  { -- | The unique identifier for the recommender model configuration. This
    -- identifier is displayed as the __Recommender ID__ on the Amazon Pinpoint
    -- console.
    recommenderId :: Prelude.Text,
    updateRecommenderConfiguration :: UpdateRecommenderConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateRecommenderConfiguration'' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'recommenderId', 'updateRecommenderConfiguration'_recommenderId' - The unique identifier for the recommender model configuration. This
-- identifier is displayed as the __Recommender ID__ on the Amazon Pinpoint
-- console.
--
-- 'updateRecommenderConfiguration', 'updateRecommenderConfiguration'_updateRecommenderConfiguration' - Undocumented member.
newUpdateRecommenderConfiguration' ::
  -- | 'recommenderId'
  Prelude.Text ->
  -- | 'updateRecommenderConfiguration'
  UpdateRecommenderConfiguration ->
  UpdateRecommenderConfiguration'
newUpdateRecommenderConfiguration'
  pRecommenderId_
  pUpdateRecommenderConfiguration_ =
    UpdateRecommenderConfiguration''
      { recommenderId =
          pRecommenderId_,
        updateRecommenderConfiguration =
          pUpdateRecommenderConfiguration_
      }

-- | The unique identifier for the recommender model configuration. This
-- identifier is displayed as the __Recommender ID__ on the Amazon Pinpoint
-- console.
updateRecommenderConfiguration'_recommenderId :: Lens.Lens' UpdateRecommenderConfiguration' Prelude.Text
updateRecommenderConfiguration'_recommenderId = Lens.lens (\UpdateRecommenderConfiguration'' {recommenderId} -> recommenderId) (\s@UpdateRecommenderConfiguration'' {} a -> s {recommenderId = a} :: UpdateRecommenderConfiguration')

-- | Undocumented member.
updateRecommenderConfiguration'_updateRecommenderConfiguration :: Lens.Lens' UpdateRecommenderConfiguration' UpdateRecommenderConfiguration
updateRecommenderConfiguration'_updateRecommenderConfiguration = Lens.lens (\UpdateRecommenderConfiguration'' {updateRecommenderConfiguration} -> updateRecommenderConfiguration) (\s@UpdateRecommenderConfiguration'' {} a -> s {updateRecommenderConfiguration = a} :: UpdateRecommenderConfiguration')

instance
  Core.AWSRequest
    UpdateRecommenderConfiguration'
  where
  type
    AWSResponse UpdateRecommenderConfiguration' =
      UpdateRecommenderConfigurationResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateRecommenderConfigurationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Core.eitherParseJSON x)
      )

instance
  Prelude.Hashable
    UpdateRecommenderConfiguration'

instance
  Prelude.NFData
    UpdateRecommenderConfiguration'

instance
  Core.ToHeaders
    UpdateRecommenderConfiguration'
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateRecommenderConfiguration' where
  toJSON UpdateRecommenderConfiguration'' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "UpdateRecommenderConfiguration"
                  Core..= updateRecommenderConfiguration
              )
          ]
      )

instance Core.ToPath UpdateRecommenderConfiguration' where
  toPath UpdateRecommenderConfiguration'' {..} =
    Prelude.mconcat
      ["/v1/recommenders/", Core.toBS recommenderId]

instance Core.ToQuery UpdateRecommenderConfiguration' where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateRecommenderConfigurationResponse' smart constructor.
data UpdateRecommenderConfigurationResponse = UpdateRecommenderConfigurationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    recommenderConfigurationResponse :: RecommenderConfigurationResponse
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateRecommenderConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateRecommenderConfigurationResponse_httpStatus' - The response's http status code.
--
-- 'recommenderConfigurationResponse', 'updateRecommenderConfigurationResponse_recommenderConfigurationResponse' - Undocumented member.
newUpdateRecommenderConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'recommenderConfigurationResponse'
  RecommenderConfigurationResponse ->
  UpdateRecommenderConfigurationResponse
newUpdateRecommenderConfigurationResponse
  pHttpStatus_
  pRecommenderConfigurationResponse_ =
    UpdateRecommenderConfigurationResponse'
      { httpStatus =
          pHttpStatus_,
        recommenderConfigurationResponse =
          pRecommenderConfigurationResponse_
      }

-- | The response's http status code.
updateRecommenderConfigurationResponse_httpStatus :: Lens.Lens' UpdateRecommenderConfigurationResponse Prelude.Int
updateRecommenderConfigurationResponse_httpStatus = Lens.lens (\UpdateRecommenderConfigurationResponse' {httpStatus} -> httpStatus) (\s@UpdateRecommenderConfigurationResponse' {} a -> s {httpStatus = a} :: UpdateRecommenderConfigurationResponse)

-- | Undocumented member.
updateRecommenderConfigurationResponse_recommenderConfigurationResponse :: Lens.Lens' UpdateRecommenderConfigurationResponse RecommenderConfigurationResponse
updateRecommenderConfigurationResponse_recommenderConfigurationResponse = Lens.lens (\UpdateRecommenderConfigurationResponse' {recommenderConfigurationResponse} -> recommenderConfigurationResponse) (\s@UpdateRecommenderConfigurationResponse' {} a -> s {recommenderConfigurationResponse = a} :: UpdateRecommenderConfigurationResponse)

instance
  Prelude.NFData
    UpdateRecommenderConfigurationResponse
