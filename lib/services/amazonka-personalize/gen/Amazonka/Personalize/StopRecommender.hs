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
-- Module      : Amazonka.Personalize.StopRecommender
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops a recommender that is ACTIVE. Stopping a recommender halts billing
-- and automatic retraining for the recommender.
module Amazonka.Personalize.StopRecommender
  ( -- * Creating a Request
    StopRecommender (..),
    newStopRecommender,

    -- * Request Lenses
    stopRecommender_recommenderArn,

    -- * Destructuring the Response
    StopRecommenderResponse (..),
    newStopRecommenderResponse,

    -- * Response Lenses
    stopRecommenderResponse_recommenderArn,
    stopRecommenderResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Personalize.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStopRecommender' smart constructor.
data StopRecommender = StopRecommender'
  { -- | The Amazon Resource Name (ARN) of the recommender to stop.
    recommenderArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopRecommender' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'recommenderArn', 'stopRecommender_recommenderArn' - The Amazon Resource Name (ARN) of the recommender to stop.
newStopRecommender ::
  -- | 'recommenderArn'
  Prelude.Text ->
  StopRecommender
newStopRecommender pRecommenderArn_ =
  StopRecommender' {recommenderArn = pRecommenderArn_}

-- | The Amazon Resource Name (ARN) of the recommender to stop.
stopRecommender_recommenderArn :: Lens.Lens' StopRecommender Prelude.Text
stopRecommender_recommenderArn = Lens.lens (\StopRecommender' {recommenderArn} -> recommenderArn) (\s@StopRecommender' {} a -> s {recommenderArn = a} :: StopRecommender)

instance Core.AWSRequest StopRecommender where
  type
    AWSResponse StopRecommender =
      StopRecommenderResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StopRecommenderResponse'
            Prelude.<$> (x Data..?> "recommenderArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StopRecommender where
  hashWithSalt _salt StopRecommender' {..} =
    _salt `Prelude.hashWithSalt` recommenderArn

instance Prelude.NFData StopRecommender where
  rnf StopRecommender' {..} = Prelude.rnf recommenderArn

instance Data.ToHeaders StopRecommender where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonPersonalize.StopRecommender" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StopRecommender where
  toJSON StopRecommender' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("recommenderArn" Data..= recommenderArn)
          ]
      )

instance Data.ToPath StopRecommender where
  toPath = Prelude.const "/"

instance Data.ToQuery StopRecommender where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStopRecommenderResponse' smart constructor.
data StopRecommenderResponse = StopRecommenderResponse'
  { -- | The Amazon Resource Name (ARN) of the recommender you stopped.
    recommenderArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopRecommenderResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'recommenderArn', 'stopRecommenderResponse_recommenderArn' - The Amazon Resource Name (ARN) of the recommender you stopped.
--
-- 'httpStatus', 'stopRecommenderResponse_httpStatus' - The response's http status code.
newStopRecommenderResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StopRecommenderResponse
newStopRecommenderResponse pHttpStatus_ =
  StopRecommenderResponse'
    { recommenderArn =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the recommender you stopped.
stopRecommenderResponse_recommenderArn :: Lens.Lens' StopRecommenderResponse (Prelude.Maybe Prelude.Text)
stopRecommenderResponse_recommenderArn = Lens.lens (\StopRecommenderResponse' {recommenderArn} -> recommenderArn) (\s@StopRecommenderResponse' {} a -> s {recommenderArn = a} :: StopRecommenderResponse)

-- | The response's http status code.
stopRecommenderResponse_httpStatus :: Lens.Lens' StopRecommenderResponse Prelude.Int
stopRecommenderResponse_httpStatus = Lens.lens (\StopRecommenderResponse' {httpStatus} -> httpStatus) (\s@StopRecommenderResponse' {} a -> s {httpStatus = a} :: StopRecommenderResponse)

instance Prelude.NFData StopRecommenderResponse where
  rnf StopRecommenderResponse' {..} =
    Prelude.rnf recommenderArn
      `Prelude.seq` Prelude.rnf httpStatus
