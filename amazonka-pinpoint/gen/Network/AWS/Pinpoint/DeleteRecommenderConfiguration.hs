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
-- Module      : Network.AWS.Pinpoint.DeleteRecommenderConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an Amazon Pinpoint configuration for a recommender model.
module Network.AWS.Pinpoint.DeleteRecommenderConfiguration
  ( -- * Creating a Request
    DeleteRecommenderConfiguration (..),
    newDeleteRecommenderConfiguration,

    -- * Request Lenses
    deleteRecommenderConfiguration_recommenderId,

    -- * Destructuring the Response
    DeleteRecommenderConfigurationResponse (..),
    newDeleteRecommenderConfigurationResponse,

    -- * Response Lenses
    deleteRecommenderConfigurationResponse_httpStatus,
    deleteRecommenderConfigurationResponse_recommenderConfigurationResponse,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteRecommenderConfiguration' smart constructor.
data DeleteRecommenderConfiguration = DeleteRecommenderConfiguration'
  { -- | The unique identifier for the recommender model configuration. This
    -- identifier is displayed as the __Recommender ID__ on the Amazon Pinpoint
    -- console.
    recommenderId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteRecommenderConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'recommenderId', 'deleteRecommenderConfiguration_recommenderId' - The unique identifier for the recommender model configuration. This
-- identifier is displayed as the __Recommender ID__ on the Amazon Pinpoint
-- console.
newDeleteRecommenderConfiguration ::
  -- | 'recommenderId'
  Prelude.Text ->
  DeleteRecommenderConfiguration
newDeleteRecommenderConfiguration pRecommenderId_ =
  DeleteRecommenderConfiguration'
    { recommenderId =
        pRecommenderId_
    }

-- | The unique identifier for the recommender model configuration. This
-- identifier is displayed as the __Recommender ID__ on the Amazon Pinpoint
-- console.
deleteRecommenderConfiguration_recommenderId :: Lens.Lens' DeleteRecommenderConfiguration Prelude.Text
deleteRecommenderConfiguration_recommenderId = Lens.lens (\DeleteRecommenderConfiguration' {recommenderId} -> recommenderId) (\s@DeleteRecommenderConfiguration' {} a -> s {recommenderId = a} :: DeleteRecommenderConfiguration)

instance
  Core.AWSRequest
    DeleteRecommenderConfiguration
  where
  type
    AWSResponse DeleteRecommenderConfiguration =
      DeleteRecommenderConfigurationResponse
  request = Request.delete defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteRecommenderConfigurationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Core.eitherParseJSON x)
      )

instance
  Prelude.Hashable
    DeleteRecommenderConfiguration

instance
  Prelude.NFData
    DeleteRecommenderConfiguration

instance
  Core.ToHeaders
    DeleteRecommenderConfiguration
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

instance Core.ToPath DeleteRecommenderConfiguration where
  toPath DeleteRecommenderConfiguration' {..} =
    Prelude.mconcat
      ["/v1/recommenders/", Core.toBS recommenderId]

instance Core.ToQuery DeleteRecommenderConfiguration where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteRecommenderConfigurationResponse' smart constructor.
data DeleteRecommenderConfigurationResponse = DeleteRecommenderConfigurationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    recommenderConfigurationResponse :: RecommenderConfigurationResponse
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteRecommenderConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteRecommenderConfigurationResponse_httpStatus' - The response's http status code.
--
-- 'recommenderConfigurationResponse', 'deleteRecommenderConfigurationResponse_recommenderConfigurationResponse' - Undocumented member.
newDeleteRecommenderConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'recommenderConfigurationResponse'
  RecommenderConfigurationResponse ->
  DeleteRecommenderConfigurationResponse
newDeleteRecommenderConfigurationResponse
  pHttpStatus_
  pRecommenderConfigurationResponse_ =
    DeleteRecommenderConfigurationResponse'
      { httpStatus =
          pHttpStatus_,
        recommenderConfigurationResponse =
          pRecommenderConfigurationResponse_
      }

-- | The response's http status code.
deleteRecommenderConfigurationResponse_httpStatus :: Lens.Lens' DeleteRecommenderConfigurationResponse Prelude.Int
deleteRecommenderConfigurationResponse_httpStatus = Lens.lens (\DeleteRecommenderConfigurationResponse' {httpStatus} -> httpStatus) (\s@DeleteRecommenderConfigurationResponse' {} a -> s {httpStatus = a} :: DeleteRecommenderConfigurationResponse)

-- | Undocumented member.
deleteRecommenderConfigurationResponse_recommenderConfigurationResponse :: Lens.Lens' DeleteRecommenderConfigurationResponse RecommenderConfigurationResponse
deleteRecommenderConfigurationResponse_recommenderConfigurationResponse = Lens.lens (\DeleteRecommenderConfigurationResponse' {recommenderConfigurationResponse} -> recommenderConfigurationResponse) (\s@DeleteRecommenderConfigurationResponse' {} a -> s {recommenderConfigurationResponse = a} :: DeleteRecommenderConfigurationResponse)

instance
  Prelude.NFData
    DeleteRecommenderConfigurationResponse
