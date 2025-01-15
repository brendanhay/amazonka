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
-- Module      : Amazonka.WAFRegional.ListResourcesForWebACL
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
module Amazonka.WAFRegional.ListResourcesForWebACL
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WAFRegional.Types

-- | /See:/ 'newListResourcesForWebACL' smart constructor.
data ListResourcesForWebACL = ListResourcesForWebACL'
  { -- | The type of resource to list, either an application load balancer or
    -- Amazon API Gateway.
    resourceType :: Prelude.Maybe ResourceType,
    -- | The unique identifier (ID) of the web ACL for which to list the
    -- associated resources.
    webACLId :: Prelude.Text
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
-- 'resourceType', 'listResourcesForWebACL_resourceType' - The type of resource to list, either an application load balancer or
-- Amazon API Gateway.
--
-- 'webACLId', 'listResourcesForWebACL_webACLId' - The unique identifier (ID) of the web ACL for which to list the
-- associated resources.
newListResourcesForWebACL ::
  -- | 'webACLId'
  Prelude.Text ->
  ListResourcesForWebACL
newListResourcesForWebACL pWebACLId_ =
  ListResourcesForWebACL'
    { resourceType =
        Prelude.Nothing,
      webACLId = pWebACLId_
    }

-- | The type of resource to list, either an application load balancer or
-- Amazon API Gateway.
listResourcesForWebACL_resourceType :: Lens.Lens' ListResourcesForWebACL (Prelude.Maybe ResourceType)
listResourcesForWebACL_resourceType = Lens.lens (\ListResourcesForWebACL' {resourceType} -> resourceType) (\s@ListResourcesForWebACL' {} a -> s {resourceType = a} :: ListResourcesForWebACL)

-- | The unique identifier (ID) of the web ACL for which to list the
-- associated resources.
listResourcesForWebACL_webACLId :: Lens.Lens' ListResourcesForWebACL Prelude.Text
listResourcesForWebACL_webACLId = Lens.lens (\ListResourcesForWebACL' {webACLId} -> webACLId) (\s@ListResourcesForWebACL' {} a -> s {webACLId = a} :: ListResourcesForWebACL)

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
    _salt
      `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` webACLId

instance Prelude.NFData ListResourcesForWebACL where
  rnf ListResourcesForWebACL' {..} =
    Prelude.rnf resourceType `Prelude.seq`
      Prelude.rnf webACLId

instance Data.ToHeaders ListResourcesForWebACL where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSWAF_Regional_20161128.ListResourcesForWebACL" ::
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
            Prelude.Just ("WebACLId" Data..= webACLId)
          ]
      )

instance Data.ToPath ListResourcesForWebACL where
  toPath = Prelude.const "/"

instance Data.ToQuery ListResourcesForWebACL where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListResourcesForWebACLResponse' smart constructor.
data ListResourcesForWebACLResponse = ListResourcesForWebACLResponse'
  { -- | An array of ARNs (Amazon Resource Names) of the resources associated
    -- with the specified web ACL. An array with zero elements is returned if
    -- there are no resources associated with the web ACL.
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
-- 'resourceArns', 'listResourcesForWebACLResponse_resourceArns' - An array of ARNs (Amazon Resource Names) of the resources associated
-- with the specified web ACL. An array with zero elements is returned if
-- there are no resources associated with the web ACL.
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

-- | An array of ARNs (Amazon Resource Names) of the resources associated
-- with the specified web ACL. An array with zero elements is returned if
-- there are no resources associated with the web ACL.
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
    Prelude.rnf resourceArns `Prelude.seq`
      Prelude.rnf httpStatus
