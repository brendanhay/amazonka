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
-- Module      : Amazonka.Personalize.DescribeRecommender
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the given recommender, including its status.
--
-- A recommender can be in one of the following states:
--
-- -   CREATE PENDING > CREATE IN_PROGRESS > ACTIVE -or- CREATE FAILED
--
-- -   STOP PENDING > STOP IN_PROGRESS > INACTIVE > START PENDING > START
--     IN_PROGRESS > ACTIVE
--
-- -   DELETE PENDING > DELETE IN_PROGRESS
--
-- When the @status@ is @CREATE FAILED@, the response includes the
-- @failureReason@ key, which describes why.
--
-- The @modelMetrics@ key is null when the recommender is being created or
-- deleted.
--
-- For more information on recommenders, see
-- <https://docs.aws.amazon.com/personalize/latest/dg/API_CreateRecommender.html CreateRecommender>.
module Amazonka.Personalize.DescribeRecommender
  ( -- * Creating a Request
    DescribeRecommender (..),
    newDescribeRecommender,

    -- * Request Lenses
    describeRecommender_recommenderArn,

    -- * Destructuring the Response
    DescribeRecommenderResponse (..),
    newDescribeRecommenderResponse,

    -- * Response Lenses
    describeRecommenderResponse_recommender,
    describeRecommenderResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Personalize.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeRecommender' smart constructor.
data DescribeRecommender = DescribeRecommender'
  { -- | The Amazon Resource Name (ARN) of the recommender to describe.
    recommenderArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeRecommender' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'recommenderArn', 'describeRecommender_recommenderArn' - The Amazon Resource Name (ARN) of the recommender to describe.
newDescribeRecommender ::
  -- | 'recommenderArn'
  Prelude.Text ->
  DescribeRecommender
newDescribeRecommender pRecommenderArn_ =
  DescribeRecommender'
    { recommenderArn =
        pRecommenderArn_
    }

-- | The Amazon Resource Name (ARN) of the recommender to describe.
describeRecommender_recommenderArn :: Lens.Lens' DescribeRecommender Prelude.Text
describeRecommender_recommenderArn = Lens.lens (\DescribeRecommender' {recommenderArn} -> recommenderArn) (\s@DescribeRecommender' {} a -> s {recommenderArn = a} :: DescribeRecommender)

instance Core.AWSRequest DescribeRecommender where
  type
    AWSResponse DescribeRecommender =
      DescribeRecommenderResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeRecommenderResponse'
            Prelude.<$> (x Data..?> "recommender")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeRecommender where
  hashWithSalt _salt DescribeRecommender' {..} =
    _salt `Prelude.hashWithSalt` recommenderArn

instance Prelude.NFData DescribeRecommender where
  rnf DescribeRecommender' {..} =
    Prelude.rnf recommenderArn

instance Data.ToHeaders DescribeRecommender where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonPersonalize.DescribeRecommender" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeRecommender where
  toJSON DescribeRecommender' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("recommenderArn" Data..= recommenderArn)
          ]
      )

instance Data.ToPath DescribeRecommender where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeRecommender where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeRecommenderResponse' smart constructor.
data DescribeRecommenderResponse = DescribeRecommenderResponse'
  { -- | The properties of the recommender.
    recommender :: Prelude.Maybe Recommender,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeRecommenderResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'recommender', 'describeRecommenderResponse_recommender' - The properties of the recommender.
--
-- 'httpStatus', 'describeRecommenderResponse_httpStatus' - The response's http status code.
newDescribeRecommenderResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeRecommenderResponse
newDescribeRecommenderResponse pHttpStatus_ =
  DescribeRecommenderResponse'
    { recommender =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The properties of the recommender.
describeRecommenderResponse_recommender :: Lens.Lens' DescribeRecommenderResponse (Prelude.Maybe Recommender)
describeRecommenderResponse_recommender = Lens.lens (\DescribeRecommenderResponse' {recommender} -> recommender) (\s@DescribeRecommenderResponse' {} a -> s {recommender = a} :: DescribeRecommenderResponse)

-- | The response's http status code.
describeRecommenderResponse_httpStatus :: Lens.Lens' DescribeRecommenderResponse Prelude.Int
describeRecommenderResponse_httpStatus = Lens.lens (\DescribeRecommenderResponse' {httpStatus} -> httpStatus) (\s@DescribeRecommenderResponse' {} a -> s {httpStatus = a} :: DescribeRecommenderResponse)

instance Prelude.NFData DescribeRecommenderResponse where
  rnf DescribeRecommenderResponse' {..} =
    Prelude.rnf recommender
      `Prelude.seq` Prelude.rnf httpStatus
