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
-- Module      : Network.AWS.Route53.CreateTrafficPolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a traffic policy, which you use to create multiple DNS resource
-- record sets for one domain name (such as example.com) or one subdomain
-- name (such as www.example.com).
module Network.AWS.Route53.CreateTrafficPolicy
  ( -- * Creating a Request
    CreateTrafficPolicy (..),
    newCreateTrafficPolicy,

    -- * Request Lenses
    createTrafficPolicy_comment,
    createTrafficPolicy_name,
    createTrafficPolicy_document,

    -- * Destructuring the Response
    CreateTrafficPolicyResponse (..),
    newCreateTrafficPolicyResponse,

    -- * Response Lenses
    createTrafficPolicyResponse_httpStatus,
    createTrafficPolicyResponse_trafficPolicy,
    createTrafficPolicyResponse_location,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Route53.Types

-- | A complex type that contains information about the traffic policy that
-- you want to create.
--
-- /See:/ 'newCreateTrafficPolicy' smart constructor.
data CreateTrafficPolicy = CreateTrafficPolicy'
  { -- | (Optional) Any comments that you want to include about the traffic
    -- policy.
    comment :: Core.Maybe Core.Text,
    -- | The name of the traffic policy.
    name :: Core.Text,
    -- | The definition of this traffic policy in JSON format. For more
    -- information, see
    -- <https://docs.aws.amazon.com/Route53/latest/APIReference/api-policies-traffic-policy-document-format.html Traffic Policy Document Format>.
    document :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateTrafficPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'comment', 'createTrafficPolicy_comment' - (Optional) Any comments that you want to include about the traffic
-- policy.
--
-- 'name', 'createTrafficPolicy_name' - The name of the traffic policy.
--
-- 'document', 'createTrafficPolicy_document' - The definition of this traffic policy in JSON format. For more
-- information, see
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/api-policies-traffic-policy-document-format.html Traffic Policy Document Format>.
newCreateTrafficPolicy ::
  -- | 'name'
  Core.Text ->
  -- | 'document'
  Core.Text ->
  CreateTrafficPolicy
newCreateTrafficPolicy pName_ pDocument_ =
  CreateTrafficPolicy'
    { comment = Core.Nothing,
      name = pName_,
      document = pDocument_
    }

-- | (Optional) Any comments that you want to include about the traffic
-- policy.
createTrafficPolicy_comment :: Lens.Lens' CreateTrafficPolicy (Core.Maybe Core.Text)
createTrafficPolicy_comment = Lens.lens (\CreateTrafficPolicy' {comment} -> comment) (\s@CreateTrafficPolicy' {} a -> s {comment = a} :: CreateTrafficPolicy)

-- | The name of the traffic policy.
createTrafficPolicy_name :: Lens.Lens' CreateTrafficPolicy Core.Text
createTrafficPolicy_name = Lens.lens (\CreateTrafficPolicy' {name} -> name) (\s@CreateTrafficPolicy' {} a -> s {name = a} :: CreateTrafficPolicy)

-- | The definition of this traffic policy in JSON format. For more
-- information, see
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/api-policies-traffic-policy-document-format.html Traffic Policy Document Format>.
createTrafficPolicy_document :: Lens.Lens' CreateTrafficPolicy Core.Text
createTrafficPolicy_document = Lens.lens (\CreateTrafficPolicy' {document} -> document) (\s@CreateTrafficPolicy' {} a -> s {document = a} :: CreateTrafficPolicy)

instance Core.AWSRequest CreateTrafficPolicy where
  type
    AWSResponse CreateTrafficPolicy =
      CreateTrafficPolicyResponse
  request = Request.postXML defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          CreateTrafficPolicyResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..@ "TrafficPolicy")
            Core.<*> (h Core..# "Location")
      )

instance Core.Hashable CreateTrafficPolicy

instance Core.NFData CreateTrafficPolicy

instance Core.ToElement CreateTrafficPolicy where
  toElement =
    Core.mkElement
      "{https://route53.amazonaws.com/doc/2013-04-01/}CreateTrafficPolicyRequest"

instance Core.ToHeaders CreateTrafficPolicy where
  toHeaders = Core.const Core.mempty

instance Core.ToPath CreateTrafficPolicy where
  toPath = Core.const "/2013-04-01/trafficpolicy"

instance Core.ToQuery CreateTrafficPolicy where
  toQuery = Core.const Core.mempty

instance Core.ToXML CreateTrafficPolicy where
  toXML CreateTrafficPolicy' {..} =
    Core.mconcat
      [ "Comment" Core.@= comment,
        "Name" Core.@= name,
        "Document" Core.@= document
      ]

-- | A complex type that contains the response information for the
-- @CreateTrafficPolicy@ request.
--
-- /See:/ 'newCreateTrafficPolicyResponse' smart constructor.
data CreateTrafficPolicyResponse = CreateTrafficPolicyResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | A complex type that contains settings for the new traffic policy.
    trafficPolicy :: TrafficPolicy,
    -- | A unique URL that represents a new traffic policy.
    location :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateTrafficPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createTrafficPolicyResponse_httpStatus' - The response's http status code.
--
-- 'trafficPolicy', 'createTrafficPolicyResponse_trafficPolicy' - A complex type that contains settings for the new traffic policy.
--
-- 'location', 'createTrafficPolicyResponse_location' - A unique URL that represents a new traffic policy.
newCreateTrafficPolicyResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'trafficPolicy'
  TrafficPolicy ->
  -- | 'location'
  Core.Text ->
  CreateTrafficPolicyResponse
newCreateTrafficPolicyResponse
  pHttpStatus_
  pTrafficPolicy_
  pLocation_ =
    CreateTrafficPolicyResponse'
      { httpStatus =
          pHttpStatus_,
        trafficPolicy = pTrafficPolicy_,
        location = pLocation_
      }

-- | The response's http status code.
createTrafficPolicyResponse_httpStatus :: Lens.Lens' CreateTrafficPolicyResponse Core.Int
createTrafficPolicyResponse_httpStatus = Lens.lens (\CreateTrafficPolicyResponse' {httpStatus} -> httpStatus) (\s@CreateTrafficPolicyResponse' {} a -> s {httpStatus = a} :: CreateTrafficPolicyResponse)

-- | A complex type that contains settings for the new traffic policy.
createTrafficPolicyResponse_trafficPolicy :: Lens.Lens' CreateTrafficPolicyResponse TrafficPolicy
createTrafficPolicyResponse_trafficPolicy = Lens.lens (\CreateTrafficPolicyResponse' {trafficPolicy} -> trafficPolicy) (\s@CreateTrafficPolicyResponse' {} a -> s {trafficPolicy = a} :: CreateTrafficPolicyResponse)

-- | A unique URL that represents a new traffic policy.
createTrafficPolicyResponse_location :: Lens.Lens' CreateTrafficPolicyResponse Core.Text
createTrafficPolicyResponse_location = Lens.lens (\CreateTrafficPolicyResponse' {location} -> location) (\s@CreateTrafficPolicyResponse' {} a -> s {location = a} :: CreateTrafficPolicyResponse)

instance Core.NFData CreateTrafficPolicyResponse
