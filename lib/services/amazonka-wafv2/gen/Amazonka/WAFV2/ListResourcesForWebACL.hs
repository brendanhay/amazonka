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
-- Module      : Amazonka.WAFV2.ListResourcesForWebACL
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves an array of the Amazon Resource Names (ARNs) for the regional
-- resources that are associated with the specified web ACL. If you want
-- the list of Amazon CloudFront resources, use the CloudFront call
-- @ListDistributionsByWebACLId@.
module Amazonka.WAFV2.ListResourcesForWebACL
  ( -- * Creating a Request
    ListResourcesForWebACL (..),
    newListResourcesForWebACL,

    -- * Request Lenses
    listResourcesForWebACL_resourceType,
    listResourcesForWebACL_webACLArn,

    -- * Destructuring the Response
    ListResourcesForWebACLResponse (..),
    newListResourcesForWebACLResponse,

    -- * Response Lenses
    listResourcesForWebACLResponse_resourceArns,
    listResourcesForWebACLResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WAFV2.Types

-- | /See:/ 'newListResourcesForWebACL' smart constructor.
data ListResourcesForWebACL = ListResourcesForWebACL'
  { -- | Used for web ACLs that are scoped for regional applications. A regional
    -- application can be an Application Load Balancer (ALB), an Amazon API
    -- Gateway REST API, an AppSync GraphQL API, or an Amazon Cognito user
    -- pool.
    --
    -- If you don\'t provide a resource type, the call uses the resource type
    -- @APPLICATION_LOAD_BALANCER@.
    --
    -- Default: @APPLICATION_LOAD_BALANCER@
    resourceType :: Prelude.Maybe ResourceType,
    -- | The Amazon Resource Name (ARN) of the web ACL.
    webACLArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListResourcesForWebACL' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceType', 'listResourcesForWebACL_resourceType' - Used for web ACLs that are scoped for regional applications. A regional
-- application can be an Application Load Balancer (ALB), an Amazon API
-- Gateway REST API, an AppSync GraphQL API, or an Amazon Cognito user
-- pool.
--
-- If you don\'t provide a resource type, the call uses the resource type
-- @APPLICATION_LOAD_BALANCER@.
--
-- Default: @APPLICATION_LOAD_BALANCER@
--
-- 'webACLArn', 'listResourcesForWebACL_webACLArn' - The Amazon Resource Name (ARN) of the web ACL.
newListResourcesForWebACL ::
  -- | 'webACLArn'
  Prelude.Text ->
  ListResourcesForWebACL
newListResourcesForWebACL pWebACLArn_ =
  ListResourcesForWebACL'
    { resourceType =
        Prelude.Nothing,
      webACLArn = pWebACLArn_
    }

-- | Used for web ACLs that are scoped for regional applications. A regional
-- application can be an Application Load Balancer (ALB), an Amazon API
-- Gateway REST API, an AppSync GraphQL API, or an Amazon Cognito user
-- pool.
--
-- If you don\'t provide a resource type, the call uses the resource type
-- @APPLICATION_LOAD_BALANCER@.
--
-- Default: @APPLICATION_LOAD_BALANCER@
listResourcesForWebACL_resourceType :: Lens.Lens' ListResourcesForWebACL (Prelude.Maybe ResourceType)
listResourcesForWebACL_resourceType = Lens.lens (\ListResourcesForWebACL' {resourceType} -> resourceType) (\s@ListResourcesForWebACL' {} a -> s {resourceType = a} :: ListResourcesForWebACL)

-- | The Amazon Resource Name (ARN) of the web ACL.
listResourcesForWebACL_webACLArn :: Lens.Lens' ListResourcesForWebACL Prelude.Text
listResourcesForWebACL_webACLArn = Lens.lens (\ListResourcesForWebACL' {webACLArn} -> webACLArn) (\s@ListResourcesForWebACL' {} a -> s {webACLArn = a} :: ListResourcesForWebACL)

instance Core.AWSRequest ListResourcesForWebACL where
  type
    AWSResponse ListResourcesForWebACL =
      ListResourcesForWebACLResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListResourcesForWebACLResponse'
            Prelude.<$> (x Data..?> "ResourceArns" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListResourcesForWebACL where
  hashWithSalt _salt ListResourcesForWebACL' {..} =
    _salt `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` webACLArn

instance Prelude.NFData ListResourcesForWebACL where
  rnf ListResourcesForWebACL' {..} =
    Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf webACLArn

instance Data.ToHeaders ListResourcesForWebACL where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSWAF_20190729.ListResourcesForWebACL" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListResourcesForWebACL where
  toJSON ListResourcesForWebACL' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ResourceType" Data..=) Prelude.<$> resourceType,
            Prelude.Just ("WebACLArn" Data..= webACLArn)
          ]
      )

instance Data.ToPath ListResourcesForWebACL where
  toPath = Prelude.const "/"

instance Data.ToQuery ListResourcesForWebACL where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListResourcesForWebACLResponse' smart constructor.
data ListResourcesForWebACLResponse = ListResourcesForWebACLResponse'
  { -- | The array of Amazon Resource Names (ARNs) of the associated resources.
    resourceArns :: Prelude.Maybe [Prelude.Text],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListResourcesForWebACLResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceArns', 'listResourcesForWebACLResponse_resourceArns' - The array of Amazon Resource Names (ARNs) of the associated resources.
--
-- 'httpStatus', 'listResourcesForWebACLResponse_httpStatus' - The response's http status code.
newListResourcesForWebACLResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListResourcesForWebACLResponse
newListResourcesForWebACLResponse pHttpStatus_ =
  ListResourcesForWebACLResponse'
    { resourceArns =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The array of Amazon Resource Names (ARNs) of the associated resources.
listResourcesForWebACLResponse_resourceArns :: Lens.Lens' ListResourcesForWebACLResponse (Prelude.Maybe [Prelude.Text])
listResourcesForWebACLResponse_resourceArns = Lens.lens (\ListResourcesForWebACLResponse' {resourceArns} -> resourceArns) (\s@ListResourcesForWebACLResponse' {} a -> s {resourceArns = a} :: ListResourcesForWebACLResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listResourcesForWebACLResponse_httpStatus :: Lens.Lens' ListResourcesForWebACLResponse Prelude.Int
listResourcesForWebACLResponse_httpStatus = Lens.lens (\ListResourcesForWebACLResponse' {httpStatus} -> httpStatus) (\s@ListResourcesForWebACLResponse' {} a -> s {httpStatus = a} :: ListResourcesForWebACLResponse)

instance
  Prelude.NFData
    ListResourcesForWebACLResponse
  where
  rnf ListResourcesForWebACLResponse' {..} =
    Prelude.rnf resourceArns
      `Prelude.seq` Prelude.rnf httpStatus
