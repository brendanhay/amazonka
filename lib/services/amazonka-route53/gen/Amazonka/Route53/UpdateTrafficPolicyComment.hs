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
-- Module      : Amazonka.Route53.UpdateTrafficPolicyComment
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the comment for a specified traffic policy version.
module Amazonka.Route53.UpdateTrafficPolicyComment
  ( -- * Creating a Request
    UpdateTrafficPolicyComment (..),
    newUpdateTrafficPolicyComment,

    -- * Request Lenses
    updateTrafficPolicyComment_id,
    updateTrafficPolicyComment_version,
    updateTrafficPolicyComment_comment,

    -- * Destructuring the Response
    UpdateTrafficPolicyCommentResponse (..),
    newUpdateTrafficPolicyCommentResponse,

    -- * Response Lenses
    updateTrafficPolicyCommentResponse_httpStatus,
    updateTrafficPolicyCommentResponse_trafficPolicy,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53.Types

-- | A complex type that contains information about the traffic policy that
-- you want to update the comment for.
--
-- /See:/ 'newUpdateTrafficPolicyComment' smart constructor.
data UpdateTrafficPolicyComment = UpdateTrafficPolicyComment'
  { -- | The value of @Id@ for the traffic policy that you want to update the
    -- comment for.
    id :: Prelude.Text,
    -- | The value of @Version@ for the traffic policy that you want to update
    -- the comment for.
    version :: Prelude.Natural,
    -- | The new comment for the specified traffic policy and version.
    comment :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateTrafficPolicyComment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'updateTrafficPolicyComment_id' - The value of @Id@ for the traffic policy that you want to update the
-- comment for.
--
-- 'version', 'updateTrafficPolicyComment_version' - The value of @Version@ for the traffic policy that you want to update
-- the comment for.
--
-- 'comment', 'updateTrafficPolicyComment_comment' - The new comment for the specified traffic policy and version.
newUpdateTrafficPolicyComment ::
  -- | 'id'
  Prelude.Text ->
  -- | 'version'
  Prelude.Natural ->
  -- | 'comment'
  Prelude.Text ->
  UpdateTrafficPolicyComment
newUpdateTrafficPolicyComment
  pId_
  pVersion_
  pComment_ =
    UpdateTrafficPolicyComment'
      { id = pId_,
        version = pVersion_,
        comment = pComment_
      }

-- | The value of @Id@ for the traffic policy that you want to update the
-- comment for.
updateTrafficPolicyComment_id :: Lens.Lens' UpdateTrafficPolicyComment Prelude.Text
updateTrafficPolicyComment_id = Lens.lens (\UpdateTrafficPolicyComment' {id} -> id) (\s@UpdateTrafficPolicyComment' {} a -> s {id = a} :: UpdateTrafficPolicyComment)

-- | The value of @Version@ for the traffic policy that you want to update
-- the comment for.
updateTrafficPolicyComment_version :: Lens.Lens' UpdateTrafficPolicyComment Prelude.Natural
updateTrafficPolicyComment_version = Lens.lens (\UpdateTrafficPolicyComment' {version} -> version) (\s@UpdateTrafficPolicyComment' {} a -> s {version = a} :: UpdateTrafficPolicyComment)

-- | The new comment for the specified traffic policy and version.
updateTrafficPolicyComment_comment :: Lens.Lens' UpdateTrafficPolicyComment Prelude.Text
updateTrafficPolicyComment_comment = Lens.lens (\UpdateTrafficPolicyComment' {comment} -> comment) (\s@UpdateTrafficPolicyComment' {} a -> s {comment = a} :: UpdateTrafficPolicyComment)

instance Core.AWSRequest UpdateTrafficPolicyComment where
  type
    AWSResponse UpdateTrafficPolicyComment =
      UpdateTrafficPolicyCommentResponse
  request overrides =
    Request.postXML (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          UpdateTrafficPolicyCommentResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..@ "TrafficPolicy")
      )

instance Prelude.Hashable UpdateTrafficPolicyComment where
  hashWithSalt _salt UpdateTrafficPolicyComment' {..} =
    _salt `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` version
      `Prelude.hashWithSalt` comment

instance Prelude.NFData UpdateTrafficPolicyComment where
  rnf UpdateTrafficPolicyComment' {..} =
    Prelude.rnf id
      `Prelude.seq` Prelude.rnf version
      `Prelude.seq` Prelude.rnf comment

instance Data.ToElement UpdateTrafficPolicyComment where
  toElement =
    Data.mkElement
      "{https://route53.amazonaws.com/doc/2013-04-01/}UpdateTrafficPolicyCommentRequest"

instance Data.ToHeaders UpdateTrafficPolicyComment where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath UpdateTrafficPolicyComment where
  toPath UpdateTrafficPolicyComment' {..} =
    Prelude.mconcat
      [ "/2013-04-01/trafficpolicy/",
        Data.toBS id,
        "/",
        Data.toBS version
      ]

instance Data.ToQuery UpdateTrafficPolicyComment where
  toQuery = Prelude.const Prelude.mempty

instance Data.ToXML UpdateTrafficPolicyComment where
  toXML UpdateTrafficPolicyComment' {..} =
    Prelude.mconcat ["Comment" Data.@= comment]

-- | A complex type that contains the response information for the traffic
-- policy.
--
-- /See:/ 'newUpdateTrafficPolicyCommentResponse' smart constructor.
data UpdateTrafficPolicyCommentResponse = UpdateTrafficPolicyCommentResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A complex type that contains settings for the specified traffic policy.
    trafficPolicy :: TrafficPolicy
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateTrafficPolicyCommentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateTrafficPolicyCommentResponse_httpStatus' - The response's http status code.
--
-- 'trafficPolicy', 'updateTrafficPolicyCommentResponse_trafficPolicy' - A complex type that contains settings for the specified traffic policy.
newUpdateTrafficPolicyCommentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'trafficPolicy'
  TrafficPolicy ->
  UpdateTrafficPolicyCommentResponse
newUpdateTrafficPolicyCommentResponse
  pHttpStatus_
  pTrafficPolicy_ =
    UpdateTrafficPolicyCommentResponse'
      { httpStatus =
          pHttpStatus_,
        trafficPolicy = pTrafficPolicy_
      }

-- | The response's http status code.
updateTrafficPolicyCommentResponse_httpStatus :: Lens.Lens' UpdateTrafficPolicyCommentResponse Prelude.Int
updateTrafficPolicyCommentResponse_httpStatus = Lens.lens (\UpdateTrafficPolicyCommentResponse' {httpStatus} -> httpStatus) (\s@UpdateTrafficPolicyCommentResponse' {} a -> s {httpStatus = a} :: UpdateTrafficPolicyCommentResponse)

-- | A complex type that contains settings for the specified traffic policy.
updateTrafficPolicyCommentResponse_trafficPolicy :: Lens.Lens' UpdateTrafficPolicyCommentResponse TrafficPolicy
updateTrafficPolicyCommentResponse_trafficPolicy = Lens.lens (\UpdateTrafficPolicyCommentResponse' {trafficPolicy} -> trafficPolicy) (\s@UpdateTrafficPolicyCommentResponse' {} a -> s {trafficPolicy = a} :: UpdateTrafficPolicyCommentResponse)

instance
  Prelude.NFData
    UpdateTrafficPolicyCommentResponse
  where
  rnf UpdateTrafficPolicyCommentResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf trafficPolicy
