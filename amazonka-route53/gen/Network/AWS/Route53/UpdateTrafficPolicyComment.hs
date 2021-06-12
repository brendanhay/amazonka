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
-- Module      : Network.AWS.Route53.UpdateTrafficPolicyComment
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the comment for a specified traffic policy version.
module Network.AWS.Route53.UpdateTrafficPolicyComment
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Route53.Types

-- | A complex type that contains information about the traffic policy that
-- you want to update the comment for.
--
-- /See:/ 'newUpdateTrafficPolicyComment' smart constructor.
data UpdateTrafficPolicyComment = UpdateTrafficPolicyComment'
  { -- | The value of @Id@ for the traffic policy that you want to update the
    -- comment for.
    id :: Core.Text,
    -- | The value of @Version@ for the traffic policy that you want to update
    -- the comment for.
    version :: Core.Natural,
    -- | The new comment for the specified traffic policy and version.
    comment :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  -- | 'version'
  Core.Natural ->
  -- | 'comment'
  Core.Text ->
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
updateTrafficPolicyComment_id :: Lens.Lens' UpdateTrafficPolicyComment Core.Text
updateTrafficPolicyComment_id = Lens.lens (\UpdateTrafficPolicyComment' {id} -> id) (\s@UpdateTrafficPolicyComment' {} a -> s {id = a} :: UpdateTrafficPolicyComment)

-- | The value of @Version@ for the traffic policy that you want to update
-- the comment for.
updateTrafficPolicyComment_version :: Lens.Lens' UpdateTrafficPolicyComment Core.Natural
updateTrafficPolicyComment_version = Lens.lens (\UpdateTrafficPolicyComment' {version} -> version) (\s@UpdateTrafficPolicyComment' {} a -> s {version = a} :: UpdateTrafficPolicyComment)

-- | The new comment for the specified traffic policy and version.
updateTrafficPolicyComment_comment :: Lens.Lens' UpdateTrafficPolicyComment Core.Text
updateTrafficPolicyComment_comment = Lens.lens (\UpdateTrafficPolicyComment' {comment} -> comment) (\s@UpdateTrafficPolicyComment' {} a -> s {comment = a} :: UpdateTrafficPolicyComment)

instance Core.AWSRequest UpdateTrafficPolicyComment where
  type
    AWSResponse UpdateTrafficPolicyComment =
      UpdateTrafficPolicyCommentResponse
  request = Request.postXML defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          UpdateTrafficPolicyCommentResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..@ "TrafficPolicy")
      )

instance Core.Hashable UpdateTrafficPolicyComment

instance Core.NFData UpdateTrafficPolicyComment

instance Core.ToElement UpdateTrafficPolicyComment where
  toElement =
    Core.mkElement
      "{https://route53.amazonaws.com/doc/2013-04-01/}UpdateTrafficPolicyCommentRequest"

instance Core.ToHeaders UpdateTrafficPolicyComment where
  toHeaders = Core.const Core.mempty

instance Core.ToPath UpdateTrafficPolicyComment where
  toPath UpdateTrafficPolicyComment' {..} =
    Core.mconcat
      [ "/2013-04-01/trafficpolicy/",
        Core.toBS id,
        "/",
        Core.toBS version
      ]

instance Core.ToQuery UpdateTrafficPolicyComment where
  toQuery = Core.const Core.mempty

instance Core.ToXML UpdateTrafficPolicyComment where
  toXML UpdateTrafficPolicyComment' {..} =
    Core.mconcat ["Comment" Core.@= comment]

-- | A complex type that contains the response information for the traffic
-- policy.
--
-- /See:/ 'newUpdateTrafficPolicyCommentResponse' smart constructor.
data UpdateTrafficPolicyCommentResponse = UpdateTrafficPolicyCommentResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | A complex type that contains settings for the specified traffic policy.
    trafficPolicy :: TrafficPolicy
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
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
updateTrafficPolicyCommentResponse_httpStatus :: Lens.Lens' UpdateTrafficPolicyCommentResponse Core.Int
updateTrafficPolicyCommentResponse_httpStatus = Lens.lens (\UpdateTrafficPolicyCommentResponse' {httpStatus} -> httpStatus) (\s@UpdateTrafficPolicyCommentResponse' {} a -> s {httpStatus = a} :: UpdateTrafficPolicyCommentResponse)

-- | A complex type that contains settings for the specified traffic policy.
updateTrafficPolicyCommentResponse_trafficPolicy :: Lens.Lens' UpdateTrafficPolicyCommentResponse TrafficPolicy
updateTrafficPolicyCommentResponse_trafficPolicy = Lens.lens (\UpdateTrafficPolicyCommentResponse' {trafficPolicy} -> trafficPolicy) (\s@UpdateTrafficPolicyCommentResponse' {} a -> s {trafficPolicy = a} :: UpdateTrafficPolicyCommentResponse)

instance
  Core.NFData
    UpdateTrafficPolicyCommentResponse
