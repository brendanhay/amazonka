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
-- Module      : Network.AWS.Route53.CreateTrafficPolicyVersion
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new version of an existing traffic policy. When you create a
-- new version of a traffic policy, you specify the ID of the traffic
-- policy that you want to update and a JSON-formatted document that
-- describes the new version. You use traffic policies to create multiple
-- DNS resource record sets for one domain name (such as example.com) or
-- one subdomain name (such as www.example.com). You can create a maximum
-- of 1000 versions of a traffic policy. If you reach the limit and need to
-- create another version, you\'ll need to start a new traffic policy.
module Network.AWS.Route53.CreateTrafficPolicyVersion
  ( -- * Creating a Request
    CreateTrafficPolicyVersion (..),
    newCreateTrafficPolicyVersion,

    -- * Request Lenses
    createTrafficPolicyVersion_comment,
    createTrafficPolicyVersion_id,
    createTrafficPolicyVersion_document,

    -- * Destructuring the Response
    CreateTrafficPolicyVersionResponse (..),
    newCreateTrafficPolicyVersionResponse,

    -- * Response Lenses
    createTrafficPolicyVersionResponse_httpStatus,
    createTrafficPolicyVersionResponse_trafficPolicy,
    createTrafficPolicyVersionResponse_location,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Route53.Types

-- | A complex type that contains information about the traffic policy that
-- you want to create a new version for.
--
-- /See:/ 'newCreateTrafficPolicyVersion' smart constructor.
data CreateTrafficPolicyVersion = CreateTrafficPolicyVersion'
  { -- | The comment that you specified in the @CreateTrafficPolicyVersion@
    -- request, if any.
    comment :: Core.Maybe Core.Text,
    -- | The ID of the traffic policy for which you want to create a new version.
    id :: Core.Text,
    -- | The definition of this version of the traffic policy, in JSON format.
    -- You specified the JSON in the @CreateTrafficPolicyVersion@ request. For
    -- more information about the JSON format, see
    -- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_CreateTrafficPolicy.html CreateTrafficPolicy>.
    document :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateTrafficPolicyVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'comment', 'createTrafficPolicyVersion_comment' - The comment that you specified in the @CreateTrafficPolicyVersion@
-- request, if any.
--
-- 'id', 'createTrafficPolicyVersion_id' - The ID of the traffic policy for which you want to create a new version.
--
-- 'document', 'createTrafficPolicyVersion_document' - The definition of this version of the traffic policy, in JSON format.
-- You specified the JSON in the @CreateTrafficPolicyVersion@ request. For
-- more information about the JSON format, see
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_CreateTrafficPolicy.html CreateTrafficPolicy>.
newCreateTrafficPolicyVersion ::
  -- | 'id'
  Core.Text ->
  -- | 'document'
  Core.Text ->
  CreateTrafficPolicyVersion
newCreateTrafficPolicyVersion pId_ pDocument_ =
  CreateTrafficPolicyVersion'
    { comment = Core.Nothing,
      id = pId_,
      document = pDocument_
    }

-- | The comment that you specified in the @CreateTrafficPolicyVersion@
-- request, if any.
createTrafficPolicyVersion_comment :: Lens.Lens' CreateTrafficPolicyVersion (Core.Maybe Core.Text)
createTrafficPolicyVersion_comment = Lens.lens (\CreateTrafficPolicyVersion' {comment} -> comment) (\s@CreateTrafficPolicyVersion' {} a -> s {comment = a} :: CreateTrafficPolicyVersion)

-- | The ID of the traffic policy for which you want to create a new version.
createTrafficPolicyVersion_id :: Lens.Lens' CreateTrafficPolicyVersion Core.Text
createTrafficPolicyVersion_id = Lens.lens (\CreateTrafficPolicyVersion' {id} -> id) (\s@CreateTrafficPolicyVersion' {} a -> s {id = a} :: CreateTrafficPolicyVersion)

-- | The definition of this version of the traffic policy, in JSON format.
-- You specified the JSON in the @CreateTrafficPolicyVersion@ request. For
-- more information about the JSON format, see
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_CreateTrafficPolicy.html CreateTrafficPolicy>.
createTrafficPolicyVersion_document :: Lens.Lens' CreateTrafficPolicyVersion Core.Text
createTrafficPolicyVersion_document = Lens.lens (\CreateTrafficPolicyVersion' {document} -> document) (\s@CreateTrafficPolicyVersion' {} a -> s {document = a} :: CreateTrafficPolicyVersion)

instance Core.AWSRequest CreateTrafficPolicyVersion where
  type
    AWSResponse CreateTrafficPolicyVersion =
      CreateTrafficPolicyVersionResponse
  request = Request.postXML defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          CreateTrafficPolicyVersionResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..@ "TrafficPolicy")
            Core.<*> (h Core..# "Location")
      )

instance Core.Hashable CreateTrafficPolicyVersion

instance Core.NFData CreateTrafficPolicyVersion

instance Core.ToElement CreateTrafficPolicyVersion where
  toElement =
    Core.mkElement
      "{https://route53.amazonaws.com/doc/2013-04-01/}CreateTrafficPolicyVersionRequest"

instance Core.ToHeaders CreateTrafficPolicyVersion where
  toHeaders = Core.const Core.mempty

instance Core.ToPath CreateTrafficPolicyVersion where
  toPath CreateTrafficPolicyVersion' {..} =
    Core.mconcat
      ["/2013-04-01/trafficpolicy/", Core.toBS id]

instance Core.ToQuery CreateTrafficPolicyVersion where
  toQuery = Core.const Core.mempty

instance Core.ToXML CreateTrafficPolicyVersion where
  toXML CreateTrafficPolicyVersion' {..} =
    Core.mconcat
      [ "Comment" Core.@= comment,
        "Document" Core.@= document
      ]

-- | A complex type that contains the response information for the
-- @CreateTrafficPolicyVersion@ request.
--
-- /See:/ 'newCreateTrafficPolicyVersionResponse' smart constructor.
data CreateTrafficPolicyVersionResponse = CreateTrafficPolicyVersionResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | A complex type that contains settings for the new version of the traffic
    -- policy.
    trafficPolicy :: TrafficPolicy,
    -- | A unique URL that represents a new traffic policy version.
    location :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateTrafficPolicyVersionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createTrafficPolicyVersionResponse_httpStatus' - The response's http status code.
--
-- 'trafficPolicy', 'createTrafficPolicyVersionResponse_trafficPolicy' - A complex type that contains settings for the new version of the traffic
-- policy.
--
-- 'location', 'createTrafficPolicyVersionResponse_location' - A unique URL that represents a new traffic policy version.
newCreateTrafficPolicyVersionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'trafficPolicy'
  TrafficPolicy ->
  -- | 'location'
  Core.Text ->
  CreateTrafficPolicyVersionResponse
newCreateTrafficPolicyVersionResponse
  pHttpStatus_
  pTrafficPolicy_
  pLocation_ =
    CreateTrafficPolicyVersionResponse'
      { httpStatus =
          pHttpStatus_,
        trafficPolicy = pTrafficPolicy_,
        location = pLocation_
      }

-- | The response's http status code.
createTrafficPolicyVersionResponse_httpStatus :: Lens.Lens' CreateTrafficPolicyVersionResponse Core.Int
createTrafficPolicyVersionResponse_httpStatus = Lens.lens (\CreateTrafficPolicyVersionResponse' {httpStatus} -> httpStatus) (\s@CreateTrafficPolicyVersionResponse' {} a -> s {httpStatus = a} :: CreateTrafficPolicyVersionResponse)

-- | A complex type that contains settings for the new version of the traffic
-- policy.
createTrafficPolicyVersionResponse_trafficPolicy :: Lens.Lens' CreateTrafficPolicyVersionResponse TrafficPolicy
createTrafficPolicyVersionResponse_trafficPolicy = Lens.lens (\CreateTrafficPolicyVersionResponse' {trafficPolicy} -> trafficPolicy) (\s@CreateTrafficPolicyVersionResponse' {} a -> s {trafficPolicy = a} :: CreateTrafficPolicyVersionResponse)

-- | A unique URL that represents a new traffic policy version.
createTrafficPolicyVersionResponse_location :: Lens.Lens' CreateTrafficPolicyVersionResponse Core.Text
createTrafficPolicyVersionResponse_location = Lens.lens (\CreateTrafficPolicyVersionResponse' {location} -> location) (\s@CreateTrafficPolicyVersionResponse' {} a -> s {location = a} :: CreateTrafficPolicyVersionResponse)

instance
  Core.NFData
    CreateTrafficPolicyVersionResponse
