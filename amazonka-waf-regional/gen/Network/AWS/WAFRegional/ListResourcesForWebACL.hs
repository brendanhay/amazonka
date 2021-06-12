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
-- Module      : Network.AWS.WAFRegional.ListResourcesForWebACL
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This is __AWS WAF Classic Regional__ documentation. For more
-- information, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/classic-waf-chapter.html AWS WAF Classic>
-- in the developer guide.
--
-- __For the latest version of AWS WAF__, use the AWS WAFV2 API and see the
-- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-chapter.html AWS WAF Developer Guide>.
-- With the latest version, AWS WAF has a single set of endpoints for
-- regional and global use.
--
-- Returns an array of resources associated with the specified web ACL.
module Network.AWS.WAFRegional.ListResourcesForWebACL
  ( -- * Creating a Request
    ListResourcesForWebACL (..),
    newListResourcesForWebACL,

    -- * Request Lenses
    listResourcesForWebACL_resourceType,
    listResourcesForWebACL_webACLId,

    -- * Destructuring the Response
    ListResourcesForWebACLResponse (..),
    newListResourcesForWebACLResponse,

    -- * Response Lenses
    listResourcesForWebACLResponse_resourceArns,
    listResourcesForWebACLResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WAFRegional.Types

-- | /See:/ 'newListResourcesForWebACL' smart constructor.
data ListResourcesForWebACL = ListResourcesForWebACL'
  { -- | The type of resource to list, either an application load balancer or
    -- Amazon API Gateway.
    resourceType :: Core.Maybe ResourceType,
    -- | The unique identifier (ID) of the web ACL for which to list the
    -- associated resources.
    webACLId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListResourcesForWebACL' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceType', 'listResourcesForWebACL_resourceType' - The type of resource to list, either an application load balancer or
-- Amazon API Gateway.
--
-- 'webACLId', 'listResourcesForWebACL_webACLId' - The unique identifier (ID) of the web ACL for which to list the
-- associated resources.
newListResourcesForWebACL ::
  -- | 'webACLId'
  Core.Text ->
  ListResourcesForWebACL
newListResourcesForWebACL pWebACLId_ =
  ListResourcesForWebACL'
    { resourceType =
        Core.Nothing,
      webACLId = pWebACLId_
    }

-- | The type of resource to list, either an application load balancer or
-- Amazon API Gateway.
listResourcesForWebACL_resourceType :: Lens.Lens' ListResourcesForWebACL (Core.Maybe ResourceType)
listResourcesForWebACL_resourceType = Lens.lens (\ListResourcesForWebACL' {resourceType} -> resourceType) (\s@ListResourcesForWebACL' {} a -> s {resourceType = a} :: ListResourcesForWebACL)

-- | The unique identifier (ID) of the web ACL for which to list the
-- associated resources.
listResourcesForWebACL_webACLId :: Lens.Lens' ListResourcesForWebACL Core.Text
listResourcesForWebACL_webACLId = Lens.lens (\ListResourcesForWebACL' {webACLId} -> webACLId) (\s@ListResourcesForWebACL' {} a -> s {webACLId = a} :: ListResourcesForWebACL)

instance Core.AWSRequest ListResourcesForWebACL where
  type
    AWSResponse ListResourcesForWebACL =
      ListResourcesForWebACLResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListResourcesForWebACLResponse'
            Core.<$> (x Core..?> "ResourceArns" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListResourcesForWebACL

instance Core.NFData ListResourcesForWebACL

instance Core.ToHeaders ListResourcesForWebACL where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSWAF_Regional_20161128.ListResourcesForWebACL" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListResourcesForWebACL where
  toJSON ListResourcesForWebACL' {..} =
    Core.object
      ( Core.catMaybes
          [ ("ResourceType" Core..=) Core.<$> resourceType,
            Core.Just ("WebACLId" Core..= webACLId)
          ]
      )

instance Core.ToPath ListResourcesForWebACL where
  toPath = Core.const "/"

instance Core.ToQuery ListResourcesForWebACL where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListResourcesForWebACLResponse' smart constructor.
data ListResourcesForWebACLResponse = ListResourcesForWebACLResponse'
  { -- | An array of ARNs (Amazon Resource Names) of the resources associated
    -- with the specified web ACL. An array with zero elements is returned if
    -- there are no resources associated with the web ACL.
    resourceArns :: Core.Maybe [Core.Text],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListResourcesForWebACLResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceArns', 'listResourcesForWebACLResponse_resourceArns' - An array of ARNs (Amazon Resource Names) of the resources associated
-- with the specified web ACL. An array with zero elements is returned if
-- there are no resources associated with the web ACL.
--
-- 'httpStatus', 'listResourcesForWebACLResponse_httpStatus' - The response's http status code.
newListResourcesForWebACLResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListResourcesForWebACLResponse
newListResourcesForWebACLResponse pHttpStatus_ =
  ListResourcesForWebACLResponse'
    { resourceArns =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of ARNs (Amazon Resource Names) of the resources associated
-- with the specified web ACL. An array with zero elements is returned if
-- there are no resources associated with the web ACL.
listResourcesForWebACLResponse_resourceArns :: Lens.Lens' ListResourcesForWebACLResponse (Core.Maybe [Core.Text])
listResourcesForWebACLResponse_resourceArns = Lens.lens (\ListResourcesForWebACLResponse' {resourceArns} -> resourceArns) (\s@ListResourcesForWebACLResponse' {} a -> s {resourceArns = a} :: ListResourcesForWebACLResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listResourcesForWebACLResponse_httpStatus :: Lens.Lens' ListResourcesForWebACLResponse Core.Int
listResourcesForWebACLResponse_httpStatus = Lens.lens (\ListResourcesForWebACLResponse' {httpStatus} -> httpStatus) (\s@ListResourcesForWebACLResponse' {} a -> s {httpStatus = a} :: ListResourcesForWebACLResponse)

instance Core.NFData ListResourcesForWebACLResponse
