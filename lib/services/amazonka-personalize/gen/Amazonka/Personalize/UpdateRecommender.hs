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
-- Module      : Amazonka.Personalize.UpdateRecommender
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the recommender to modify the recommender configuration.
module Amazonka.Personalize.UpdateRecommender
  ( -- * Creating a Request
    UpdateRecommender (..),
    newUpdateRecommender,

    -- * Request Lenses
    updateRecommender_recommenderArn,
    updateRecommender_recommenderConfig,

    -- * Destructuring the Response
    UpdateRecommenderResponse (..),
    newUpdateRecommenderResponse,

    -- * Response Lenses
    updateRecommenderResponse_recommenderArn,
    updateRecommenderResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Personalize.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateRecommender' smart constructor.
data UpdateRecommender = UpdateRecommender'
  { -- | The Amazon Resource Name (ARN) of the recommender to modify.
    recommenderArn :: Prelude.Text,
    -- | The configuration details of the recommender.
    recommenderConfig :: RecommenderConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateRecommender' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'recommenderArn', 'updateRecommender_recommenderArn' - The Amazon Resource Name (ARN) of the recommender to modify.
--
-- 'recommenderConfig', 'updateRecommender_recommenderConfig' - The configuration details of the recommender.
newUpdateRecommender ::
  -- | 'recommenderArn'
  Prelude.Text ->
  -- | 'recommenderConfig'
  RecommenderConfig ->
  UpdateRecommender
newUpdateRecommender
  pRecommenderArn_
  pRecommenderConfig_ =
    UpdateRecommender'
      { recommenderArn =
          pRecommenderArn_,
        recommenderConfig = pRecommenderConfig_
      }

-- | The Amazon Resource Name (ARN) of the recommender to modify.
updateRecommender_recommenderArn :: Lens.Lens' UpdateRecommender Prelude.Text
updateRecommender_recommenderArn = Lens.lens (\UpdateRecommender' {recommenderArn} -> recommenderArn) (\s@UpdateRecommender' {} a -> s {recommenderArn = a} :: UpdateRecommender)

-- | The configuration details of the recommender.
updateRecommender_recommenderConfig :: Lens.Lens' UpdateRecommender RecommenderConfig
updateRecommender_recommenderConfig = Lens.lens (\UpdateRecommender' {recommenderConfig} -> recommenderConfig) (\s@UpdateRecommender' {} a -> s {recommenderConfig = a} :: UpdateRecommender)

instance Core.AWSRequest UpdateRecommender where
  type
    AWSResponse UpdateRecommender =
      UpdateRecommenderResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateRecommenderResponse'
            Prelude.<$> (x Core..?> "recommenderArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateRecommender where
  hashWithSalt _salt UpdateRecommender' {..} =
    _salt `Prelude.hashWithSalt` recommenderArn
      `Prelude.hashWithSalt` recommenderConfig

instance Prelude.NFData UpdateRecommender where
  rnf UpdateRecommender' {..} =
    Prelude.rnf recommenderArn
      `Prelude.seq` Prelude.rnf recommenderConfig

instance Core.ToHeaders UpdateRecommender where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonPersonalize.UpdateRecommender" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateRecommender where
  toJSON UpdateRecommender' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("recommenderArn" Core..= recommenderArn),
            Prelude.Just
              ("recommenderConfig" Core..= recommenderConfig)
          ]
      )

instance Core.ToPath UpdateRecommender where
  toPath = Prelude.const "/"

instance Core.ToQuery UpdateRecommender where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateRecommenderResponse' smart constructor.
data UpdateRecommenderResponse = UpdateRecommenderResponse'
  { -- | The same recommender Amazon Resource Name (ARN) as given in the request.
    recommenderArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateRecommenderResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'recommenderArn', 'updateRecommenderResponse_recommenderArn' - The same recommender Amazon Resource Name (ARN) as given in the request.
--
-- 'httpStatus', 'updateRecommenderResponse_httpStatus' - The response's http status code.
newUpdateRecommenderResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateRecommenderResponse
newUpdateRecommenderResponse pHttpStatus_ =
  UpdateRecommenderResponse'
    { recommenderArn =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The same recommender Amazon Resource Name (ARN) as given in the request.
updateRecommenderResponse_recommenderArn :: Lens.Lens' UpdateRecommenderResponse (Prelude.Maybe Prelude.Text)
updateRecommenderResponse_recommenderArn = Lens.lens (\UpdateRecommenderResponse' {recommenderArn} -> recommenderArn) (\s@UpdateRecommenderResponse' {} a -> s {recommenderArn = a} :: UpdateRecommenderResponse)

-- | The response's http status code.
updateRecommenderResponse_httpStatus :: Lens.Lens' UpdateRecommenderResponse Prelude.Int
updateRecommenderResponse_httpStatus = Lens.lens (\UpdateRecommenderResponse' {httpStatus} -> httpStatus) (\s@UpdateRecommenderResponse' {} a -> s {httpStatus = a} :: UpdateRecommenderResponse)

instance Prelude.NFData UpdateRecommenderResponse where
  rnf UpdateRecommenderResponse' {..} =
    Prelude.rnf recommenderArn
      `Prelude.seq` Prelude.rnf httpStatus
