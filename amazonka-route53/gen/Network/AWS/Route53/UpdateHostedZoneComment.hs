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
-- Module      : Network.AWS.Route53.UpdateHostedZoneComment
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the comment for a specified hosted zone.
module Network.AWS.Route53.UpdateHostedZoneComment
  ( -- * Creating a Request
    UpdateHostedZoneComment (..),
    newUpdateHostedZoneComment,

    -- * Request Lenses
    updateHostedZoneComment_comment,
    updateHostedZoneComment_id,

    -- * Destructuring the Response
    UpdateHostedZoneCommentResponse (..),
    newUpdateHostedZoneCommentResponse,

    -- * Response Lenses
    updateHostedZoneCommentResponse_httpStatus,
    updateHostedZoneCommentResponse_hostedZone,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Route53.Types

-- | A request to update the comment for a hosted zone.
--
-- /See:/ 'newUpdateHostedZoneComment' smart constructor.
data UpdateHostedZoneComment = UpdateHostedZoneComment'
  { -- | The new comment for the hosted zone. If you don\'t specify a value for
    -- @Comment@, Amazon Route 53 deletes the existing value of the @Comment@
    -- element, if any.
    comment :: Core.Maybe Core.Text,
    -- | The ID for the hosted zone that you want to update the comment for.
    id :: ResourceId
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateHostedZoneComment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'comment', 'updateHostedZoneComment_comment' - The new comment for the hosted zone. If you don\'t specify a value for
-- @Comment@, Amazon Route 53 deletes the existing value of the @Comment@
-- element, if any.
--
-- 'id', 'updateHostedZoneComment_id' - The ID for the hosted zone that you want to update the comment for.
newUpdateHostedZoneComment ::
  -- | 'id'
  ResourceId ->
  UpdateHostedZoneComment
newUpdateHostedZoneComment pId_ =
  UpdateHostedZoneComment'
    { comment = Core.Nothing,
      id = pId_
    }

-- | The new comment for the hosted zone. If you don\'t specify a value for
-- @Comment@, Amazon Route 53 deletes the existing value of the @Comment@
-- element, if any.
updateHostedZoneComment_comment :: Lens.Lens' UpdateHostedZoneComment (Core.Maybe Core.Text)
updateHostedZoneComment_comment = Lens.lens (\UpdateHostedZoneComment' {comment} -> comment) (\s@UpdateHostedZoneComment' {} a -> s {comment = a} :: UpdateHostedZoneComment)

-- | The ID for the hosted zone that you want to update the comment for.
updateHostedZoneComment_id :: Lens.Lens' UpdateHostedZoneComment ResourceId
updateHostedZoneComment_id = Lens.lens (\UpdateHostedZoneComment' {id} -> id) (\s@UpdateHostedZoneComment' {} a -> s {id = a} :: UpdateHostedZoneComment)

instance Core.AWSRequest UpdateHostedZoneComment where
  type
    AWSResponse UpdateHostedZoneComment =
      UpdateHostedZoneCommentResponse
  request = Request.postXML defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          UpdateHostedZoneCommentResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..@ "HostedZone")
      )

instance Core.Hashable UpdateHostedZoneComment

instance Core.NFData UpdateHostedZoneComment

instance Core.ToElement UpdateHostedZoneComment where
  toElement =
    Core.mkElement
      "{https://route53.amazonaws.com/doc/2013-04-01/}UpdateHostedZoneCommentRequest"

instance Core.ToHeaders UpdateHostedZoneComment where
  toHeaders = Core.const Core.mempty

instance Core.ToPath UpdateHostedZoneComment where
  toPath UpdateHostedZoneComment' {..} =
    Core.mconcat
      ["/2013-04-01/hostedzone/", Core.toBS id]

instance Core.ToQuery UpdateHostedZoneComment where
  toQuery = Core.const Core.mempty

instance Core.ToXML UpdateHostedZoneComment where
  toXML UpdateHostedZoneComment' {..} =
    Core.mconcat ["Comment" Core.@= comment]

-- | A complex type that contains the response to the
-- @UpdateHostedZoneComment@ request.
--
-- /See:/ 'newUpdateHostedZoneCommentResponse' smart constructor.
data UpdateHostedZoneCommentResponse = UpdateHostedZoneCommentResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | A complex type that contains the response to the
    -- @UpdateHostedZoneComment@ request.
    hostedZone :: HostedZone
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateHostedZoneCommentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateHostedZoneCommentResponse_httpStatus' - The response's http status code.
--
-- 'hostedZone', 'updateHostedZoneCommentResponse_hostedZone' - A complex type that contains the response to the
-- @UpdateHostedZoneComment@ request.
newUpdateHostedZoneCommentResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'hostedZone'
  HostedZone ->
  UpdateHostedZoneCommentResponse
newUpdateHostedZoneCommentResponse
  pHttpStatus_
  pHostedZone_ =
    UpdateHostedZoneCommentResponse'
      { httpStatus =
          pHttpStatus_,
        hostedZone = pHostedZone_
      }

-- | The response's http status code.
updateHostedZoneCommentResponse_httpStatus :: Lens.Lens' UpdateHostedZoneCommentResponse Core.Int
updateHostedZoneCommentResponse_httpStatus = Lens.lens (\UpdateHostedZoneCommentResponse' {httpStatus} -> httpStatus) (\s@UpdateHostedZoneCommentResponse' {} a -> s {httpStatus = a} :: UpdateHostedZoneCommentResponse)

-- | A complex type that contains the response to the
-- @UpdateHostedZoneComment@ request.
updateHostedZoneCommentResponse_hostedZone :: Lens.Lens' UpdateHostedZoneCommentResponse HostedZone
updateHostedZoneCommentResponse_hostedZone = Lens.lens (\UpdateHostedZoneCommentResponse' {hostedZone} -> hostedZone) (\s@UpdateHostedZoneCommentResponse' {} a -> s {hostedZone = a} :: UpdateHostedZoneCommentResponse)

instance Core.NFData UpdateHostedZoneCommentResponse
