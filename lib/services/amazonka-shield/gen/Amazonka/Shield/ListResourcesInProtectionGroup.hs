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
-- Module      : Amazonka.Shield.ListResourcesInProtectionGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the resources that are included in the protection group.
module Amazonka.Shield.ListResourcesInProtectionGroup
  ( -- * Creating a Request
    ListResourcesInProtectionGroup (..),
    newListResourcesInProtectionGroup,

    -- * Request Lenses
    listResourcesInProtectionGroup_maxResults,
    listResourcesInProtectionGroup_nextToken,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Shield.Types

-- | /See:/ 'newListResourcesInProtectionGroup' smart constructor.
data ListResourcesInProtectionGroup = ListResourcesInProtectionGroup'
  { -- | The greatest number of objects that you want Shield Advanced to return
    -- to the list request. Shield Advanced might return fewer objects than you
    -- indicate in this setting, even if more objects are available. If there
    -- are more objects remaining, Shield Advanced will always also return a
    -- @NextToken@ value in the response.
    --
    -- The default setting is 20.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | When you request a list of objects from Shield Advanced, if the response
    -- does not include all of the remaining available objects, Shield Advanced
    -- includes a @NextToken@ value in the response. You can retrieve the next
    -- batch of objects by requesting the list again and providing the token
    -- that was returned by the prior call in your request.
    --
    -- You can indicate the maximum number of objects that you want Shield
    -- Advanced to return for a single call with the @MaxResults@ setting.
    -- Shield Advanced will not return more than @MaxResults@ objects, but may
    -- return fewer, even if more objects are still available.
    --
    -- Whenever more objects remain that Shield Advanced has not yet returned
    -- to you, the response will include a @NextToken@ value.
    --
    -- On your first call to a list operation, leave this setting empty.
    nextToken :: Prelude.Maybe Prelude.Text,
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
-- 'maxResults', 'listResourcesInProtectionGroup_maxResults' - The greatest number of objects that you want Shield Advanced to return
-- to the list request. Shield Advanced might return fewer objects than you
-- indicate in this setting, even if more objects are available. If there
-- are more objects remaining, Shield Advanced will always also return a
-- @NextToken@ value in the response.
--
-- The default setting is 20.
--
-- 'nextToken', 'listResourcesInProtectionGroup_nextToken' - When you request a list of objects from Shield Advanced, if the response
-- does not include all of the remaining available objects, Shield Advanced
-- includes a @NextToken@ value in the response. You can retrieve the next
-- batch of objects by requesting the list again and providing the token
-- that was returned by the prior call in your request.
--
-- You can indicate the maximum number of objects that you want Shield
-- Advanced to return for a single call with the @MaxResults@ setting.
-- Shield Advanced will not return more than @MaxResults@ objects, but may
-- return fewer, even if more objects are still available.
--
-- Whenever more objects remain that Shield Advanced has not yet returned
-- to you, the response will include a @NextToken@ value.
--
-- On your first call to a list operation, leave this setting empty.
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
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      protectionGroupId = pProtectionGroupId_
    }

-- | The greatest number of objects that you want Shield Advanced to return
-- to the list request. Shield Advanced might return fewer objects than you
-- indicate in this setting, even if more objects are available. If there
-- are more objects remaining, Shield Advanced will always also return a
-- @NextToken@ value in the response.
--
-- The default setting is 20.
listResourcesInProtectionGroup_maxResults :: Lens.Lens' ListResourcesInProtectionGroup (Prelude.Maybe Prelude.Natural)
listResourcesInProtectionGroup_maxResults = Lens.lens (\ListResourcesInProtectionGroup' {maxResults} -> maxResults) (\s@ListResourcesInProtectionGroup' {} a -> s {maxResults = a} :: ListResourcesInProtectionGroup)

-- | When you request a list of objects from Shield Advanced, if the response
-- does not include all of the remaining available objects, Shield Advanced
-- includes a @NextToken@ value in the response. You can retrieve the next
-- batch of objects by requesting the list again and providing the token
-- that was returned by the prior call in your request.
--
-- You can indicate the maximum number of objects that you want Shield
-- Advanced to return for a single call with the @MaxResults@ setting.
-- Shield Advanced will not return more than @MaxResults@ objects, but may
-- return fewer, even if more objects are still available.
--
-- Whenever more objects remain that Shield Advanced has not yet returned
-- to you, the response will include a @NextToken@ value.
--
-- On your first call to a list operation, leave this setting empty.
listResourcesInProtectionGroup_nextToken :: Lens.Lens' ListResourcesInProtectionGroup (Prelude.Maybe Prelude.Text)
listResourcesInProtectionGroup_nextToken = Lens.lens (\ListResourcesInProtectionGroup' {nextToken} -> nextToken) (\s@ListResourcesInProtectionGroup' {} a -> s {nextToken = a} :: ListResourcesInProtectionGroup)

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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListResourcesInProtectionGroupResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "ResourceArns" Core..!@ Prelude.mempty)
      )

instance
  Prelude.Hashable
    ListResourcesInProtectionGroup
  where
  hashWithSalt
    _salt
    ListResourcesInProtectionGroup' {..} =
      _salt
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` protectionGroupId

instance
  Prelude.NFData
    ListResourcesInProtectionGroup
  where
  rnf ListResourcesInProtectionGroup' {..} =
    Prelude.rnf maxResults `Prelude.seq`
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf protectionGroupId

instance
  Data.ToHeaders
    ListResourcesInProtectionGroup
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSShield_20160616.ListResourcesInProtectionGroup" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListResourcesInProtectionGroup where
  toJSON ListResourcesInProtectionGroup' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just
              ("ProtectionGroupId" Data..= protectionGroupId)
          ]
      )

instance Data.ToPath ListResourcesInProtectionGroup where
  toPath = Prelude.const "/"

instance Data.ToQuery ListResourcesInProtectionGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListResourcesInProtectionGroupResponse' smart constructor.
data ListResourcesInProtectionGroupResponse = ListResourcesInProtectionGroupResponse'
  { -- | When you request a list of objects from Shield Advanced, if the response
    -- does not include all of the remaining available objects, Shield Advanced
    -- includes a @NextToken@ value in the response. You can retrieve the next
    -- batch of objects by requesting the list again and providing the token
    -- that was returned by the prior call in your request.
    --
    -- You can indicate the maximum number of objects that you want Shield
    -- Advanced to return for a single call with the @MaxResults@ setting.
    -- Shield Advanced will not return more than @MaxResults@ objects, but may
    -- return fewer, even if more objects are still available.
    --
    -- Whenever more objects remain that Shield Advanced has not yet returned
    -- to you, the response will include a @NextToken@ value.
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
-- 'nextToken', 'listResourcesInProtectionGroupResponse_nextToken' - When you request a list of objects from Shield Advanced, if the response
-- does not include all of the remaining available objects, Shield Advanced
-- includes a @NextToken@ value in the response. You can retrieve the next
-- batch of objects by requesting the list again and providing the token
-- that was returned by the prior call in your request.
--
-- You can indicate the maximum number of objects that you want Shield
-- Advanced to return for a single call with the @MaxResults@ setting.
-- Shield Advanced will not return more than @MaxResults@ objects, but may
-- return fewer, even if more objects are still available.
--
-- Whenever more objects remain that Shield Advanced has not yet returned
-- to you, the response will include a @NextToken@ value.
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

-- | When you request a list of objects from Shield Advanced, if the response
-- does not include all of the remaining available objects, Shield Advanced
-- includes a @NextToken@ value in the response. You can retrieve the next
-- batch of objects by requesting the list again and providing the token
-- that was returned by the prior call in your request.
--
-- You can indicate the maximum number of objects that you want Shield
-- Advanced to return for a single call with the @MaxResults@ setting.
-- Shield Advanced will not return more than @MaxResults@ objects, but may
-- return fewer, even if more objects are still available.
--
-- Whenever more objects remain that Shield Advanced has not yet returned
-- to you, the response will include a @NextToken@ value.
listResourcesInProtectionGroupResponse_nextToken :: Lens.Lens' ListResourcesInProtectionGroupResponse (Prelude.Maybe Prelude.Text)
listResourcesInProtectionGroupResponse_nextToken = Lens.lens (\ListResourcesInProtectionGroupResponse' {nextToken} -> nextToken) (\s@ListResourcesInProtectionGroupResponse' {} a -> s {nextToken = a} :: ListResourcesInProtectionGroupResponse)

-- | The response's http status code.
listResourcesInProtectionGroupResponse_httpStatus :: Lens.Lens' ListResourcesInProtectionGroupResponse Prelude.Int
listResourcesInProtectionGroupResponse_httpStatus = Lens.lens (\ListResourcesInProtectionGroupResponse' {httpStatus} -> httpStatus) (\s@ListResourcesInProtectionGroupResponse' {} a -> s {httpStatus = a} :: ListResourcesInProtectionGroupResponse)

-- | The Amazon Resource Names (ARNs) of the resources that are included in
-- the protection group.
listResourcesInProtectionGroupResponse_resourceArns :: Lens.Lens' ListResourcesInProtectionGroupResponse [Prelude.Text]
listResourcesInProtectionGroupResponse_resourceArns = Lens.lens (\ListResourcesInProtectionGroupResponse' {resourceArns} -> resourceArns) (\s@ListResourcesInProtectionGroupResponse' {} a -> s {resourceArns = a} :: ListResourcesInProtectionGroupResponse) Prelude.. Lens.coerced

instance
  Prelude.NFData
    ListResourcesInProtectionGroupResponse
  where
  rnf ListResourcesInProtectionGroupResponse' {..} =
    Prelude.rnf nextToken `Prelude.seq`
      Prelude.rnf httpStatus `Prelude.seq`
        Prelude.rnf resourceArns
