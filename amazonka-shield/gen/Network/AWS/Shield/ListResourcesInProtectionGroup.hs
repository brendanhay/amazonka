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
-- Module      : Network.AWS.Shield.ListResourcesInProtectionGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the resources that are included in the protection group.
module Network.AWS.Shield.ListResourcesInProtectionGroup
  ( -- * Creating a Request
    ListResourcesInProtectionGroup (..),
    newListResourcesInProtectionGroup,

    -- * Request Lenses
    listResourcesInProtectionGroup_nextToken,
    listResourcesInProtectionGroup_maxResults,
    listResourcesInProtectionGroup_protectionGroupId,

    -- * Destructuring the Response
    ListResourcesInProtectionGroupResponse (..),
    newListResourcesInProtectionGroupResponse,

    -- * Response Lenses
    listResourcesInProtectionGroupResponse_nextToken,
    listResourcesInProtectionGroupResponse_httpStatus,
    listResourcesInProtectionGroupResponse_resourceArns,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Shield.Types

-- | /See:/ 'newListResourcesInProtectionGroup' smart constructor.
data ListResourcesInProtectionGroup = ListResourcesInProtectionGroup'
  { -- | The next token value from a previous call to
    -- @ListResourcesInProtectionGroup@. Pass null if this is the first call.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of resource ARN objects to return. If you leave this
    -- blank, Shield Advanced returns the first 20 results.
    --
    -- This is a maximum value. Shield Advanced might return the results in
    -- smaller batches. That is, the number of objects returned could be less
    -- than @MaxResults@, even if there are still more objects yet to return.
    -- If there are more objects to return, Shield Advanced returns a value in
    -- @NextToken@ that you can use in your next request, to get the next batch
    -- of objects.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The name of the protection group. You use this to identify the
    -- protection group in lists and to manage the protection group, for
    -- example to update, delete, or describe it.
    protectionGroupId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListResourcesInProtectionGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listResourcesInProtectionGroup_nextToken' - The next token value from a previous call to
-- @ListResourcesInProtectionGroup@. Pass null if this is the first call.
--
-- 'maxResults', 'listResourcesInProtectionGroup_maxResults' - The maximum number of resource ARN objects to return. If you leave this
-- blank, Shield Advanced returns the first 20 results.
--
-- This is a maximum value. Shield Advanced might return the results in
-- smaller batches. That is, the number of objects returned could be less
-- than @MaxResults@, even if there are still more objects yet to return.
-- If there are more objects to return, Shield Advanced returns a value in
-- @NextToken@ that you can use in your next request, to get the next batch
-- of objects.
--
-- 'protectionGroupId', 'listResourcesInProtectionGroup_protectionGroupId' - The name of the protection group. You use this to identify the
-- protection group in lists and to manage the protection group, for
-- example to update, delete, or describe it.
newListResourcesInProtectionGroup ::
  -- | 'protectionGroupId'
  Prelude.Text ->
  ListResourcesInProtectionGroup
newListResourcesInProtectionGroup pProtectionGroupId_ =
  ListResourcesInProtectionGroup'
    { nextToken =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      protectionGroupId = pProtectionGroupId_
    }

-- | The next token value from a previous call to
-- @ListResourcesInProtectionGroup@. Pass null if this is the first call.
listResourcesInProtectionGroup_nextToken :: Lens.Lens' ListResourcesInProtectionGroup (Prelude.Maybe Prelude.Text)
listResourcesInProtectionGroup_nextToken = Lens.lens (\ListResourcesInProtectionGroup' {nextToken} -> nextToken) (\s@ListResourcesInProtectionGroup' {} a -> s {nextToken = a} :: ListResourcesInProtectionGroup)

-- | The maximum number of resource ARN objects to return. If you leave this
-- blank, Shield Advanced returns the first 20 results.
--
-- This is a maximum value. Shield Advanced might return the results in
-- smaller batches. That is, the number of objects returned could be less
-- than @MaxResults@, even if there are still more objects yet to return.
-- If there are more objects to return, Shield Advanced returns a value in
-- @NextToken@ that you can use in your next request, to get the next batch
-- of objects.
listResourcesInProtectionGroup_maxResults :: Lens.Lens' ListResourcesInProtectionGroup (Prelude.Maybe Prelude.Natural)
listResourcesInProtectionGroup_maxResults = Lens.lens (\ListResourcesInProtectionGroup' {maxResults} -> maxResults) (\s@ListResourcesInProtectionGroup' {} a -> s {maxResults = a} :: ListResourcesInProtectionGroup)

-- | The name of the protection group. You use this to identify the
-- protection group in lists and to manage the protection group, for
-- example to update, delete, or describe it.
listResourcesInProtectionGroup_protectionGroupId :: Lens.Lens' ListResourcesInProtectionGroup Prelude.Text
listResourcesInProtectionGroup_protectionGroupId = Lens.lens (\ListResourcesInProtectionGroup' {protectionGroupId} -> protectionGroupId) (\s@ListResourcesInProtectionGroup' {} a -> s {protectionGroupId = a} :: ListResourcesInProtectionGroup)

instance
  Core.AWSRequest
    ListResourcesInProtectionGroup
  where
  type
    AWSResponse ListResourcesInProtectionGroup =
      ListResourcesInProtectionGroupResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListResourcesInProtectionGroupResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..?> "ResourceArns" Core..!@ Prelude.mempty)
      )

instance
  Prelude.Hashable
    ListResourcesInProtectionGroup

instance
  Prelude.NFData
    ListResourcesInProtectionGroup

instance
  Core.ToHeaders
    ListResourcesInProtectionGroup
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSShield_20160616.ListResourcesInProtectionGroup" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListResourcesInProtectionGroup where
  toJSON ListResourcesInProtectionGroup' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            Prelude.Just
              ("ProtectionGroupId" Core..= protectionGroupId)
          ]
      )

instance Core.ToPath ListResourcesInProtectionGroup where
  toPath = Prelude.const "/"

instance Core.ToQuery ListResourcesInProtectionGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListResourcesInProtectionGroupResponse' smart constructor.
data ListResourcesInProtectionGroupResponse = ListResourcesInProtectionGroupResponse'
  { -- | If you specify a value for @MaxResults@ and you have more resources in
    -- the protection group than the value of MaxResults, Shield Advanced
    -- returns this token that you can use in your next request, to get the
    -- next batch of objects.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The Amazon Resource Names (ARNs) of the resources that are included in
    -- the protection group.
    resourceArns :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListResourcesInProtectionGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listResourcesInProtectionGroupResponse_nextToken' - If you specify a value for @MaxResults@ and you have more resources in
-- the protection group than the value of MaxResults, Shield Advanced
-- returns this token that you can use in your next request, to get the
-- next batch of objects.
--
-- 'httpStatus', 'listResourcesInProtectionGroupResponse_httpStatus' - The response's http status code.
--
-- 'resourceArns', 'listResourcesInProtectionGroupResponse_resourceArns' - The Amazon Resource Names (ARNs) of the resources that are included in
-- the protection group.
newListResourcesInProtectionGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListResourcesInProtectionGroupResponse
newListResourcesInProtectionGroupResponse
  pHttpStatus_ =
    ListResourcesInProtectionGroupResponse'
      { nextToken =
          Prelude.Nothing,
        httpStatus = pHttpStatus_,
        resourceArns = Prelude.mempty
      }

-- | If you specify a value for @MaxResults@ and you have more resources in
-- the protection group than the value of MaxResults, Shield Advanced
-- returns this token that you can use in your next request, to get the
-- next batch of objects.
listResourcesInProtectionGroupResponse_nextToken :: Lens.Lens' ListResourcesInProtectionGroupResponse (Prelude.Maybe Prelude.Text)
listResourcesInProtectionGroupResponse_nextToken = Lens.lens (\ListResourcesInProtectionGroupResponse' {nextToken} -> nextToken) (\s@ListResourcesInProtectionGroupResponse' {} a -> s {nextToken = a} :: ListResourcesInProtectionGroupResponse)

-- | The response's http status code.
listResourcesInProtectionGroupResponse_httpStatus :: Lens.Lens' ListResourcesInProtectionGroupResponse Prelude.Int
listResourcesInProtectionGroupResponse_httpStatus = Lens.lens (\ListResourcesInProtectionGroupResponse' {httpStatus} -> httpStatus) (\s@ListResourcesInProtectionGroupResponse' {} a -> s {httpStatus = a} :: ListResourcesInProtectionGroupResponse)

-- | The Amazon Resource Names (ARNs) of the resources that are included in
-- the protection group.
listResourcesInProtectionGroupResponse_resourceArns :: Lens.Lens' ListResourcesInProtectionGroupResponse [Prelude.Text]
listResourcesInProtectionGroupResponse_resourceArns = Lens.lens (\ListResourcesInProtectionGroupResponse' {resourceArns} -> resourceArns) (\s@ListResourcesInProtectionGroupResponse' {} a -> s {resourceArns = a} :: ListResourcesInProtectionGroupResponse) Prelude.. Lens._Coerce

instance
  Prelude.NFData
    ListResourcesInProtectionGroupResponse
