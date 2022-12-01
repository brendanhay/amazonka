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
-- Module      : Amazonka.Personalize.DeleteRecommender
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deactivates and removes a recommender. A deleted recommender can no
-- longer be specified in a
-- <https://docs.aws.amazon.com/personalize/latest/dg/API_RS_GetRecommendations.html GetRecommendations>
-- request.
module Amazonka.Personalize.DeleteRecommender
  ( -- * Creating a Request
    DeleteRecommender (..),
    newDeleteRecommender,

    -- * Request Lenses
    deleteRecommender_recommenderArn,

    -- * Destructuring the Response
    DeleteRecommenderResponse (..),
    newDeleteRecommenderResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Personalize.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteRecommender' smart constructor.
data DeleteRecommender = DeleteRecommender'
  { -- | The Amazon Resource Name (ARN) of the recommender to delete.
    recommenderArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteRecommender' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'recommenderArn', 'deleteRecommender_recommenderArn' - The Amazon Resource Name (ARN) of the recommender to delete.
newDeleteRecommender ::
  -- | 'recommenderArn'
  Prelude.Text ->
  DeleteRecommender
newDeleteRecommender pRecommenderArn_ =
  DeleteRecommender'
    { recommenderArn =
        pRecommenderArn_
    }

-- | The Amazon Resource Name (ARN) of the recommender to delete.
deleteRecommender_recommenderArn :: Lens.Lens' DeleteRecommender Prelude.Text
deleteRecommender_recommenderArn = Lens.lens (\DeleteRecommender' {recommenderArn} -> recommenderArn) (\s@DeleteRecommender' {} a -> s {recommenderArn = a} :: DeleteRecommender)

instance Core.AWSRequest DeleteRecommender where
  type
    AWSResponse DeleteRecommender =
      DeleteRecommenderResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull DeleteRecommenderResponse'

instance Prelude.Hashable DeleteRecommender where
  hashWithSalt _salt DeleteRecommender' {..} =
    _salt `Prelude.hashWithSalt` recommenderArn

instance Prelude.NFData DeleteRecommender where
  rnf DeleteRecommender' {..} =
    Prelude.rnf recommenderArn

instance Core.ToHeaders DeleteRecommender where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonPersonalize.DeleteRecommender" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DeleteRecommender where
  toJSON DeleteRecommender' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("recommenderArn" Core..= recommenderArn)
          ]
      )

instance Core.ToPath DeleteRecommender where
  toPath = Prelude.const "/"

instance Core.ToQuery DeleteRecommender where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteRecommenderResponse' smart constructor.
data DeleteRecommenderResponse = DeleteRecommenderResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteRecommenderResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteRecommenderResponse ::
  DeleteRecommenderResponse
newDeleteRecommenderResponse =
  DeleteRecommenderResponse'

instance Prelude.NFData DeleteRecommenderResponse where
  rnf _ = ()
