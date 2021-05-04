{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Shield.ListProtectionGroups
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the ProtectionGroup objects for the account.
module Network.AWS.Shield.ListProtectionGroups
  ( -- * Creating a Request
    ListProtectionGroups (..),
    newListProtectionGroups,

    -- * Request Lenses
    listProtectionGroups_nextToken,
    listProtectionGroups_maxResults,

    -- * Destructuring the Response
    ListProtectionGroupsResponse (..),
    newListProtectionGroupsResponse,

    -- * Response Lenses
    listProtectionGroupsResponse_nextToken,
    listProtectionGroupsResponse_httpStatus,
    listProtectionGroupsResponse_protectionGroups,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Shield.Types

-- | /See:/ 'newListProtectionGroups' smart constructor.
data ListProtectionGroups = ListProtectionGroups'
  { -- | The next token value from a previous call to @ListProtectionGroups@.
    -- Pass null if this is the first call.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of ProtectionGroup objects to return. If you leave
    -- this blank, Shield Advanced returns the first 20 results.
    --
    -- This is a maximum value. Shield Advanced might return the results in
    -- smaller batches. That is, the number of objects returned could be less
    -- than @MaxResults@, even if there are still more objects yet to return.
    -- If there are more objects to return, Shield Advanced returns a value in
    -- @NextToken@ that you can use in your next request, to get the next batch
    -- of objects.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ListProtectionGroups' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listProtectionGroups_nextToken' - The next token value from a previous call to @ListProtectionGroups@.
-- Pass null if this is the first call.
--
-- 'maxResults', 'listProtectionGroups_maxResults' - The maximum number of ProtectionGroup objects to return. If you leave
-- this blank, Shield Advanced returns the first 20 results.
--
-- This is a maximum value. Shield Advanced might return the results in
-- smaller batches. That is, the number of objects returned could be less
-- than @MaxResults@, even if there are still more objects yet to return.
-- If there are more objects to return, Shield Advanced returns a value in
-- @NextToken@ that you can use in your next request, to get the next batch
-- of objects.
newListProtectionGroups ::
  ListProtectionGroups
newListProtectionGroups =
  ListProtectionGroups'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | The next token value from a previous call to @ListProtectionGroups@.
-- Pass null if this is the first call.
listProtectionGroups_nextToken :: Lens.Lens' ListProtectionGroups (Prelude.Maybe Prelude.Text)
listProtectionGroups_nextToken = Lens.lens (\ListProtectionGroups' {nextToken} -> nextToken) (\s@ListProtectionGroups' {} a -> s {nextToken = a} :: ListProtectionGroups)

-- | The maximum number of ProtectionGroup objects to return. If you leave
-- this blank, Shield Advanced returns the first 20 results.
--
-- This is a maximum value. Shield Advanced might return the results in
-- smaller batches. That is, the number of objects returned could be less
-- than @MaxResults@, even if there are still more objects yet to return.
-- If there are more objects to return, Shield Advanced returns a value in
-- @NextToken@ that you can use in your next request, to get the next batch
-- of objects.
listProtectionGroups_maxResults :: Lens.Lens' ListProtectionGroups (Prelude.Maybe Prelude.Natural)
listProtectionGroups_maxResults = Lens.lens (\ListProtectionGroups' {maxResults} -> maxResults) (\s@ListProtectionGroups' {} a -> s {maxResults = a} :: ListProtectionGroups)

instance Prelude.AWSRequest ListProtectionGroups where
  type
    Rs ListProtectionGroups =
      ListProtectionGroupsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListProtectionGroupsResponse'
            Prelude.<$> (x Prelude..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Prelude..?> "ProtectionGroups"
                            Prelude..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable ListProtectionGroups

instance Prelude.NFData ListProtectionGroups

instance Prelude.ToHeaders ListProtectionGroups where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSShield_20160616.ListProtectionGroups" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON ListProtectionGroups where
  toJSON ListProtectionGroups' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("NextToken" Prelude..=) Prelude.<$> nextToken,
            ("MaxResults" Prelude..=) Prelude.<$> maxResults
          ]
      )

instance Prelude.ToPath ListProtectionGroups where
  toPath = Prelude.const "/"

instance Prelude.ToQuery ListProtectionGroups where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListProtectionGroupsResponse' smart constructor.
data ListProtectionGroupsResponse = ListProtectionGroupsResponse'
  { -- | If you specify a value for @MaxResults@ and you have more protection
    -- groups than the value of MaxResults, AWS Shield Advanced returns this
    -- token that you can use in your next request, to get the next batch of
    -- objects.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    protectionGroups :: [ProtectionGroup]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ListProtectionGroupsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listProtectionGroupsResponse_nextToken' - If you specify a value for @MaxResults@ and you have more protection
-- groups than the value of MaxResults, AWS Shield Advanced returns this
-- token that you can use in your next request, to get the next batch of
-- objects.
--
-- 'httpStatus', 'listProtectionGroupsResponse_httpStatus' - The response's http status code.
--
-- 'protectionGroups', 'listProtectionGroupsResponse_protectionGroups' -
newListProtectionGroupsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListProtectionGroupsResponse
newListProtectionGroupsResponse pHttpStatus_ =
  ListProtectionGroupsResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      protectionGroups = Prelude.mempty
    }

-- | If you specify a value for @MaxResults@ and you have more protection
-- groups than the value of MaxResults, AWS Shield Advanced returns this
-- token that you can use in your next request, to get the next batch of
-- objects.
listProtectionGroupsResponse_nextToken :: Lens.Lens' ListProtectionGroupsResponse (Prelude.Maybe Prelude.Text)
listProtectionGroupsResponse_nextToken = Lens.lens (\ListProtectionGroupsResponse' {nextToken} -> nextToken) (\s@ListProtectionGroupsResponse' {} a -> s {nextToken = a} :: ListProtectionGroupsResponse)

-- | The response's http status code.
listProtectionGroupsResponse_httpStatus :: Lens.Lens' ListProtectionGroupsResponse Prelude.Int
listProtectionGroupsResponse_httpStatus = Lens.lens (\ListProtectionGroupsResponse' {httpStatus} -> httpStatus) (\s@ListProtectionGroupsResponse' {} a -> s {httpStatus = a} :: ListProtectionGroupsResponse)

-- |
listProtectionGroupsResponse_protectionGroups :: Lens.Lens' ListProtectionGroupsResponse [ProtectionGroup]
listProtectionGroupsResponse_protectionGroups = Lens.lens (\ListProtectionGroupsResponse' {protectionGroups} -> protectionGroups) (\s@ListProtectionGroupsResponse' {} a -> s {protectionGroups = a} :: ListProtectionGroupsResponse) Prelude.. Prelude._Coerce

instance Prelude.NFData ListProtectionGroupsResponse
