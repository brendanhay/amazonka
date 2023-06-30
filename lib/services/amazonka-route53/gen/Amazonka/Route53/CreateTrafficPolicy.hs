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
-- Module      : Amazonka.Route53.CreateTrafficPolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a traffic policy, which you use to create multiple DNS resource
-- record sets for one domain name (such as example.com) or one subdomain
-- name (such as www.example.com).
module Amazonka.Route53.CreateTrafficPolicy
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53.Types

-- | A complex type that contains information about the traffic policy that
-- you want to create.
--
-- /See:/ 'newCreateTrafficPolicy' smart constructor.
data CreateTrafficPolicy = CreateTrafficPolicy'
  { -- | (Optional) Any comments that you want to include about the traffic
    -- policy.
    comment :: Prelude.Maybe Prelude.Text,
    -- | The name of the traffic policy.
    name :: Prelude.Text,
    -- | The definition of this traffic policy in JSON format. For more
    -- information, see
    -- <https://docs.aws.amazon.com/Route53/latest/APIReference/api-policies-traffic-policy-document-format.html Traffic Policy Document Format>.
    document :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'document'
  Prelude.Text ->
  CreateTrafficPolicy
newCreateTrafficPolicy pName_ pDocument_ =
  CreateTrafficPolicy'
    { comment = Prelude.Nothing,
      name = pName_,
      document = pDocument_
    }

-- | (Optional) Any comments that you want to include about the traffic
-- policy.
createTrafficPolicy_comment :: Lens.Lens' CreateTrafficPolicy (Prelude.Maybe Prelude.Text)
createTrafficPolicy_comment = Lens.lens (\CreateTrafficPolicy' {comment} -> comment) (\s@CreateTrafficPolicy' {} a -> s {comment = a} :: CreateTrafficPolicy)

-- | The name of the traffic policy.
createTrafficPolicy_name :: Lens.Lens' CreateTrafficPolicy Prelude.Text
createTrafficPolicy_name = Lens.lens (\CreateTrafficPolicy' {name} -> name) (\s@CreateTrafficPolicy' {} a -> s {name = a} :: CreateTrafficPolicy)

-- | The definition of this traffic policy in JSON format. For more
-- information, see
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/api-policies-traffic-policy-document-format.html Traffic Policy Document Format>.
createTrafficPolicy_document :: Lens.Lens' CreateTrafficPolicy Prelude.Text
createTrafficPolicy_document = Lens.lens (\CreateTrafficPolicy' {document} -> document) (\s@CreateTrafficPolicy' {} a -> s {document = a} :: CreateTrafficPolicy)

instance Core.AWSRequest CreateTrafficPolicy where
  type
    AWSResponse CreateTrafficPolicy =
      CreateTrafficPolicyResponse
  request overrides =
    Request.postXML (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          CreateTrafficPolicyResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..@ "TrafficPolicy")
            Prelude.<*> (h Data..# "Location")
      )

instance Prelude.Hashable CreateTrafficPolicy where
  hashWithSalt _salt CreateTrafficPolicy' {..} =
    _salt
      `Prelude.hashWithSalt` comment
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` document

instance Prelude.NFData CreateTrafficPolicy where
  rnf CreateTrafficPolicy' {..} =
    Prelude.rnf comment
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf document

instance Data.ToElement CreateTrafficPolicy where
  toElement =
    Data.mkElement
      "{https://route53.amazonaws.com/doc/2013-04-01/}CreateTrafficPolicyRequest"

instance Data.ToHeaders CreateTrafficPolicy where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath CreateTrafficPolicy where
  toPath = Prelude.const "/2013-04-01/trafficpolicy"

instance Data.ToQuery CreateTrafficPolicy where
  toQuery = Prelude.const Prelude.mempty

instance Data.ToXML CreateTrafficPolicy where
  toXML CreateTrafficPolicy' {..} =
    Prelude.mconcat
      [ "Comment" Data.@= comment,
        "Name" Data.@= name,
        "Document" Data.@= document
      ]

-- | A complex type that contains the response information for the
-- @CreateTrafficPolicy@ request.
--
-- /See:/ 'newCreateTrafficPolicyResponse' smart constructor.
data CreateTrafficPolicyResponse = CreateTrafficPolicyResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A complex type that contains settings for the new traffic policy.
    trafficPolicy :: TrafficPolicy,
    -- | A unique URL that represents a new traffic policy.
    location :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  -- | 'trafficPolicy'
  TrafficPolicy ->
  -- | 'location'
  Prelude.Text ->
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
createTrafficPolicyResponse_httpStatus :: Lens.Lens' CreateTrafficPolicyResponse Prelude.Int
createTrafficPolicyResponse_httpStatus = Lens.lens (\CreateTrafficPolicyResponse' {httpStatus} -> httpStatus) (\s@CreateTrafficPolicyResponse' {} a -> s {httpStatus = a} :: CreateTrafficPolicyResponse)

-- | A complex type that contains settings for the new traffic policy.
createTrafficPolicyResponse_trafficPolicy :: Lens.Lens' CreateTrafficPolicyResponse TrafficPolicy
createTrafficPolicyResponse_trafficPolicy = Lens.lens (\CreateTrafficPolicyResponse' {trafficPolicy} -> trafficPolicy) (\s@CreateTrafficPolicyResponse' {} a -> s {trafficPolicy = a} :: CreateTrafficPolicyResponse)

-- | A unique URL that represents a new traffic policy.
createTrafficPolicyResponse_location :: Lens.Lens' CreateTrafficPolicyResponse Prelude.Text
createTrafficPolicyResponse_location = Lens.lens (\CreateTrafficPolicyResponse' {location} -> location) (\s@CreateTrafficPolicyResponse' {} a -> s {location = a} :: CreateTrafficPolicyResponse)

instance Prelude.NFData CreateTrafficPolicyResponse where
  rnf CreateTrafficPolicyResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf trafficPolicy
      `Prelude.seq` Prelude.rnf location
