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
-- Module      : Amazonka.WAFV2.ListAvailableManagedRuleGroupVersions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of the available versions for the specified managed rule
-- group.
module Amazonka.WAFV2.ListAvailableManagedRuleGroupVersions
  ( -- * Creating a Request
    ListAvailableManagedRuleGroupVersions (..),
    newListAvailableManagedRuleGroupVersions,

    -- * Request Lenses
    listAvailableManagedRuleGroupVersions_limit,
    listAvailableManagedRuleGroupVersions_nextMarker,
    listAvailableManagedRuleGroupVersions_vendorName,
    listAvailableManagedRuleGroupVersions_name,
    listAvailableManagedRuleGroupVersions_scope,

    -- * Destructuring the Response
    ListAvailableManagedRuleGroupVersionsResponse (..),
    newListAvailableManagedRuleGroupVersionsResponse,

    -- * Response Lenses
    listAvailableManagedRuleGroupVersionsResponse_versions,
    listAvailableManagedRuleGroupVersionsResponse_nextMarker,
    listAvailableManagedRuleGroupVersionsResponse_currentDefaultVersion,
    listAvailableManagedRuleGroupVersionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WAFV2.Types

-- | /See:/ 'newListAvailableManagedRuleGroupVersions' smart constructor.
data ListAvailableManagedRuleGroupVersions = ListAvailableManagedRuleGroupVersions'
  { -- | The maximum number of objects that you want WAF to return for this
    -- request. If more objects are available, in the response, WAF provides a
    -- @NextMarker@ value that you can use in a subsequent call to get the next
    -- batch of objects.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | When you request a list of objects with a @Limit@ setting, if the number
    -- of objects that are still available for retrieval exceeds the limit, WAF
    -- returns a @NextMarker@ value in the response. To retrieve the next batch
    -- of objects, provide the marker from the prior call in your next request.
    nextMarker :: Prelude.Maybe Prelude.Text,
    -- | The name of the managed rule group vendor. You use this, along with the
    -- rule group name, to identify the rule group.
    vendorName :: Prelude.Text,
    -- | The name of the managed rule group. You use this, along with the vendor
    -- name, to identify the rule group.
    name :: Prelude.Text,
    -- | Specifies whether this is for an Amazon CloudFront distribution or for a
    -- regional application. A regional application can be an Application Load
    -- Balancer (ALB), an Amazon API Gateway REST API, an AppSync GraphQL API,
    -- or an Amazon Cognito user pool.
    --
    -- To work with CloudFront, you must also specify the Region US East (N.
    -- Virginia) as follows:
    --
    -- -   CLI - Specify the Region when you use the CloudFront scope:
    --     @--scope=CLOUDFRONT --region=us-east-1@.
    --
    -- -   API and SDKs - For all calls, use the Region endpoint us-east-1.
    scope :: Scope
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAvailableManagedRuleGroupVersions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'limit', 'listAvailableManagedRuleGroupVersions_limit' - The maximum number of objects that you want WAF to return for this
-- request. If more objects are available, in the response, WAF provides a
-- @NextMarker@ value that you can use in a subsequent call to get the next
-- batch of objects.
--
-- 'nextMarker', 'listAvailableManagedRuleGroupVersions_nextMarker' - When you request a list of objects with a @Limit@ setting, if the number
-- of objects that are still available for retrieval exceeds the limit, WAF
-- returns a @NextMarker@ value in the response. To retrieve the next batch
-- of objects, provide the marker from the prior call in your next request.
--
-- 'vendorName', 'listAvailableManagedRuleGroupVersions_vendorName' - The name of the managed rule group vendor. You use this, along with the
-- rule group name, to identify the rule group.
--
-- 'name', 'listAvailableManagedRuleGroupVersions_name' - The name of the managed rule group. You use this, along with the vendor
-- name, to identify the rule group.
--
-- 'scope', 'listAvailableManagedRuleGroupVersions_scope' - Specifies whether this is for an Amazon CloudFront distribution or for a
-- regional application. A regional application can be an Application Load
-- Balancer (ALB), an Amazon API Gateway REST API, an AppSync GraphQL API,
-- or an Amazon Cognito user pool.
--
-- To work with CloudFront, you must also specify the Region US East (N.
-- Virginia) as follows:
--
-- -   CLI - Specify the Region when you use the CloudFront scope:
--     @--scope=CLOUDFRONT --region=us-east-1@.
--
-- -   API and SDKs - For all calls, use the Region endpoint us-east-1.
newListAvailableManagedRuleGroupVersions ::
  -- | 'vendorName'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 'scope'
  Scope ->
  ListAvailableManagedRuleGroupVersions
newListAvailableManagedRuleGroupVersions
  pVendorName_
  pName_
  pScope_ =
    ListAvailableManagedRuleGroupVersions'
      { limit =
          Prelude.Nothing,
        nextMarker = Prelude.Nothing,
        vendorName = pVendorName_,
        name = pName_,
        scope = pScope_
      }

-- | The maximum number of objects that you want WAF to return for this
-- request. If more objects are available, in the response, WAF provides a
-- @NextMarker@ value that you can use in a subsequent call to get the next
-- batch of objects.
listAvailableManagedRuleGroupVersions_limit :: Lens.Lens' ListAvailableManagedRuleGroupVersions (Prelude.Maybe Prelude.Natural)
listAvailableManagedRuleGroupVersions_limit = Lens.lens (\ListAvailableManagedRuleGroupVersions' {limit} -> limit) (\s@ListAvailableManagedRuleGroupVersions' {} a -> s {limit = a} :: ListAvailableManagedRuleGroupVersions)

-- | When you request a list of objects with a @Limit@ setting, if the number
-- of objects that are still available for retrieval exceeds the limit, WAF
-- returns a @NextMarker@ value in the response. To retrieve the next batch
-- of objects, provide the marker from the prior call in your next request.
listAvailableManagedRuleGroupVersions_nextMarker :: Lens.Lens' ListAvailableManagedRuleGroupVersions (Prelude.Maybe Prelude.Text)
listAvailableManagedRuleGroupVersions_nextMarker = Lens.lens (\ListAvailableManagedRuleGroupVersions' {nextMarker} -> nextMarker) (\s@ListAvailableManagedRuleGroupVersions' {} a -> s {nextMarker = a} :: ListAvailableManagedRuleGroupVersions)

-- | The name of the managed rule group vendor. You use this, along with the
-- rule group name, to identify the rule group.
listAvailableManagedRuleGroupVersions_vendorName :: Lens.Lens' ListAvailableManagedRuleGroupVersions Prelude.Text
listAvailableManagedRuleGroupVersions_vendorName = Lens.lens (\ListAvailableManagedRuleGroupVersions' {vendorName} -> vendorName) (\s@ListAvailableManagedRuleGroupVersions' {} a -> s {vendorName = a} :: ListAvailableManagedRuleGroupVersions)

-- | The name of the managed rule group. You use this, along with the vendor
-- name, to identify the rule group.
listAvailableManagedRuleGroupVersions_name :: Lens.Lens' ListAvailableManagedRuleGroupVersions Prelude.Text
listAvailableManagedRuleGroupVersions_name = Lens.lens (\ListAvailableManagedRuleGroupVersions' {name} -> name) (\s@ListAvailableManagedRuleGroupVersions' {} a -> s {name = a} :: ListAvailableManagedRuleGroupVersions)

-- | Specifies whether this is for an Amazon CloudFront distribution or for a
-- regional application. A regional application can be an Application Load
-- Balancer (ALB), an Amazon API Gateway REST API, an AppSync GraphQL API,
-- or an Amazon Cognito user pool.
--
-- To work with CloudFront, you must also specify the Region US East (N.
-- Virginia) as follows:
--
-- -   CLI - Specify the Region when you use the CloudFront scope:
--     @--scope=CLOUDFRONT --region=us-east-1@.
--
-- -   API and SDKs - For all calls, use the Region endpoint us-east-1.
listAvailableManagedRuleGroupVersions_scope :: Lens.Lens' ListAvailableManagedRuleGroupVersions Scope
listAvailableManagedRuleGroupVersions_scope = Lens.lens (\ListAvailableManagedRuleGroupVersions' {scope} -> scope) (\s@ListAvailableManagedRuleGroupVersions' {} a -> s {scope = a} :: ListAvailableManagedRuleGroupVersions)

instance
  Core.AWSRequest
    ListAvailableManagedRuleGroupVersions
  where
  type
    AWSResponse
      ListAvailableManagedRuleGroupVersions =
      ListAvailableManagedRuleGroupVersionsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAvailableManagedRuleGroupVersionsResponse'
            Prelude.<$> (x Core..?> "Versions" Core..!@ Prelude.mempty)
              Prelude.<*> (x Core..?> "NextMarker")
              Prelude.<*> (x Core..?> "CurrentDefaultVersion")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListAvailableManagedRuleGroupVersions
  where
  hashWithSalt
    _salt
    ListAvailableManagedRuleGroupVersions' {..} =
      _salt `Prelude.hashWithSalt` limit
        `Prelude.hashWithSalt` nextMarker
        `Prelude.hashWithSalt` vendorName
        `Prelude.hashWithSalt` name
        `Prelude.hashWithSalt` scope

instance
  Prelude.NFData
    ListAvailableManagedRuleGroupVersions
  where
  rnf ListAvailableManagedRuleGroupVersions' {..} =
    Prelude.rnf limit
      `Prelude.seq` Prelude.rnf nextMarker
      `Prelude.seq` Prelude.rnf vendorName
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf scope

instance
  Core.ToHeaders
    ListAvailableManagedRuleGroupVersions
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSWAF_20190729.ListAvailableManagedRuleGroupVersions" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Core.ToJSON
    ListAvailableManagedRuleGroupVersions
  where
  toJSON ListAvailableManagedRuleGroupVersions' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Limit" Core..=) Prelude.<$> limit,
            ("NextMarker" Core..=) Prelude.<$> nextMarker,
            Prelude.Just ("VendorName" Core..= vendorName),
            Prelude.Just ("Name" Core..= name),
            Prelude.Just ("Scope" Core..= scope)
          ]
      )

instance
  Core.ToPath
    ListAvailableManagedRuleGroupVersions
  where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    ListAvailableManagedRuleGroupVersions
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListAvailableManagedRuleGroupVersionsResponse' smart constructor.
data ListAvailableManagedRuleGroupVersionsResponse = ListAvailableManagedRuleGroupVersionsResponse'
  { -- | The versions that are currently available for the specified managed rule
    -- group.
    versions :: Prelude.Maybe [ManagedRuleGroupVersion],
    -- | When you request a list of objects with a @Limit@ setting, if the number
    -- of objects that are still available for retrieval exceeds the limit, WAF
    -- returns a @NextMarker@ value in the response. To retrieve the next batch
    -- of objects, provide the marker from the prior call in your next request.
    nextMarker :: Prelude.Maybe Prelude.Text,
    -- | The name of the version that\'s currently set as the default.
    currentDefaultVersion :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAvailableManagedRuleGroupVersionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'versions', 'listAvailableManagedRuleGroupVersionsResponse_versions' - The versions that are currently available for the specified managed rule
-- group.
--
-- 'nextMarker', 'listAvailableManagedRuleGroupVersionsResponse_nextMarker' - When you request a list of objects with a @Limit@ setting, if the number
-- of objects that are still available for retrieval exceeds the limit, WAF
-- returns a @NextMarker@ value in the response. To retrieve the next batch
-- of objects, provide the marker from the prior call in your next request.
--
-- 'currentDefaultVersion', 'listAvailableManagedRuleGroupVersionsResponse_currentDefaultVersion' - The name of the version that\'s currently set as the default.
--
-- 'httpStatus', 'listAvailableManagedRuleGroupVersionsResponse_httpStatus' - The response's http status code.
newListAvailableManagedRuleGroupVersionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListAvailableManagedRuleGroupVersionsResponse
newListAvailableManagedRuleGroupVersionsResponse
  pHttpStatus_ =
    ListAvailableManagedRuleGroupVersionsResponse'
      { versions =
          Prelude.Nothing,
        nextMarker = Prelude.Nothing,
        currentDefaultVersion =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The versions that are currently available for the specified managed rule
-- group.
listAvailableManagedRuleGroupVersionsResponse_versions :: Lens.Lens' ListAvailableManagedRuleGroupVersionsResponse (Prelude.Maybe [ManagedRuleGroupVersion])
listAvailableManagedRuleGroupVersionsResponse_versions = Lens.lens (\ListAvailableManagedRuleGroupVersionsResponse' {versions} -> versions) (\s@ListAvailableManagedRuleGroupVersionsResponse' {} a -> s {versions = a} :: ListAvailableManagedRuleGroupVersionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | When you request a list of objects with a @Limit@ setting, if the number
-- of objects that are still available for retrieval exceeds the limit, WAF
-- returns a @NextMarker@ value in the response. To retrieve the next batch
-- of objects, provide the marker from the prior call in your next request.
listAvailableManagedRuleGroupVersionsResponse_nextMarker :: Lens.Lens' ListAvailableManagedRuleGroupVersionsResponse (Prelude.Maybe Prelude.Text)
listAvailableManagedRuleGroupVersionsResponse_nextMarker = Lens.lens (\ListAvailableManagedRuleGroupVersionsResponse' {nextMarker} -> nextMarker) (\s@ListAvailableManagedRuleGroupVersionsResponse' {} a -> s {nextMarker = a} :: ListAvailableManagedRuleGroupVersionsResponse)

-- | The name of the version that\'s currently set as the default.
listAvailableManagedRuleGroupVersionsResponse_currentDefaultVersion :: Lens.Lens' ListAvailableManagedRuleGroupVersionsResponse (Prelude.Maybe Prelude.Text)
listAvailableManagedRuleGroupVersionsResponse_currentDefaultVersion = Lens.lens (\ListAvailableManagedRuleGroupVersionsResponse' {currentDefaultVersion} -> currentDefaultVersion) (\s@ListAvailableManagedRuleGroupVersionsResponse' {} a -> s {currentDefaultVersion = a} :: ListAvailableManagedRuleGroupVersionsResponse)

-- | The response's http status code.
listAvailableManagedRuleGroupVersionsResponse_httpStatus :: Lens.Lens' ListAvailableManagedRuleGroupVersionsResponse Prelude.Int
listAvailableManagedRuleGroupVersionsResponse_httpStatus = Lens.lens (\ListAvailableManagedRuleGroupVersionsResponse' {httpStatus} -> httpStatus) (\s@ListAvailableManagedRuleGroupVersionsResponse' {} a -> s {httpStatus = a} :: ListAvailableManagedRuleGroupVersionsResponse)

instance
  Prelude.NFData
    ListAvailableManagedRuleGroupVersionsResponse
  where
  rnf
    ListAvailableManagedRuleGroupVersionsResponse' {..} =
      Prelude.rnf versions
        `Prelude.seq` Prelude.rnf nextMarker
        `Prelude.seq` Prelude.rnf currentDefaultVersion
        `Prelude.seq` Prelude.rnf httpStatus
