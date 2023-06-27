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
-- Module      : Amazonka.Connect.ListContactReferences
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This API is in preview release for Amazon Connect and is subject to
-- change.
--
-- For the specified @referenceTypes@, returns a list of references
-- associated with the contact.
--
-- This operation returns paginated results.
module Amazonka.Connect.ListContactReferences
  ( -- * Creating a Request
    ListContactReferences (..),
    newListContactReferences,

    -- * Request Lenses
    listContactReferences_nextToken,
    listContactReferences_instanceId,
    listContactReferences_contactId,
    listContactReferences_referenceTypes,

    -- * Destructuring the Response
    ListContactReferencesResponse (..),
    newListContactReferencesResponse,

    -- * Response Lenses
    listContactReferencesResponse_nextToken,
    listContactReferencesResponse_referenceSummaryList,
    listContactReferencesResponse_httpStatus,
  )
where

import Amazonka.Connect.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListContactReferences' smart constructor.
data ListContactReferences = ListContactReferences'
  { -- | The token for the next set of results. Use the value returned in the
    -- previous response in the next request to retrieve the next set of
    -- results.
    --
    -- This is not expected to be set, because the value returned in the
    -- previous response is always null.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the Amazon Connect instance. You can
    -- <https://docs.aws.amazon.com/connect/latest/adminguide/find-instance-arn.html find the instance ID>
    -- in the Amazon Resource Name (ARN) of the instance.
    instanceId :: Prelude.Text,
    -- | The identifier of the initial contact.
    contactId :: Prelude.Text,
    -- | The type of reference.
    referenceTypes :: [ReferenceType]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListContactReferences' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listContactReferences_nextToken' - The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
--
-- This is not expected to be set, because the value returned in the
-- previous response is always null.
--
-- 'instanceId', 'listContactReferences_instanceId' - The identifier of the Amazon Connect instance. You can
-- <https://docs.aws.amazon.com/connect/latest/adminguide/find-instance-arn.html find the instance ID>
-- in the Amazon Resource Name (ARN) of the instance.
--
-- 'contactId', 'listContactReferences_contactId' - The identifier of the initial contact.
--
-- 'referenceTypes', 'listContactReferences_referenceTypes' - The type of reference.
newListContactReferences ::
  -- | 'instanceId'
  Prelude.Text ->
  -- | 'contactId'
  Prelude.Text ->
  ListContactReferences
newListContactReferences pInstanceId_ pContactId_ =
  ListContactReferences'
    { nextToken = Prelude.Nothing,
      instanceId = pInstanceId_,
      contactId = pContactId_,
      referenceTypes = Prelude.mempty
    }

-- | The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
--
-- This is not expected to be set, because the value returned in the
-- previous response is always null.
listContactReferences_nextToken :: Lens.Lens' ListContactReferences (Prelude.Maybe Prelude.Text)
listContactReferences_nextToken = Lens.lens (\ListContactReferences' {nextToken} -> nextToken) (\s@ListContactReferences' {} a -> s {nextToken = a} :: ListContactReferences)

-- | The identifier of the Amazon Connect instance. You can
-- <https://docs.aws.amazon.com/connect/latest/adminguide/find-instance-arn.html find the instance ID>
-- in the Amazon Resource Name (ARN) of the instance.
listContactReferences_instanceId :: Lens.Lens' ListContactReferences Prelude.Text
listContactReferences_instanceId = Lens.lens (\ListContactReferences' {instanceId} -> instanceId) (\s@ListContactReferences' {} a -> s {instanceId = a} :: ListContactReferences)

-- | The identifier of the initial contact.
listContactReferences_contactId :: Lens.Lens' ListContactReferences Prelude.Text
listContactReferences_contactId = Lens.lens (\ListContactReferences' {contactId} -> contactId) (\s@ListContactReferences' {} a -> s {contactId = a} :: ListContactReferences)

-- | The type of reference.
listContactReferences_referenceTypes :: Lens.Lens' ListContactReferences [ReferenceType]
listContactReferences_referenceTypes = Lens.lens (\ListContactReferences' {referenceTypes} -> referenceTypes) (\s@ListContactReferences' {} a -> s {referenceTypes = a} :: ListContactReferences) Prelude.. Lens.coerced

instance Core.AWSPager ListContactReferences where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listContactReferencesResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listContactReferencesResponse_referenceSummaryList
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listContactReferences_nextToken
          Lens..~ rs
          Lens.^? listContactReferencesResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListContactReferences where
  type
    AWSResponse ListContactReferences =
      ListContactReferencesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListContactReferencesResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> ( x
                            Data..?> "ReferenceSummaryList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListContactReferences where
  hashWithSalt _salt ListContactReferences' {..} =
    _salt
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` contactId
      `Prelude.hashWithSalt` referenceTypes

instance Prelude.NFData ListContactReferences where
  rnf ListContactReferences' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf contactId
      `Prelude.seq` Prelude.rnf referenceTypes

instance Data.ToHeaders ListContactReferences where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListContactReferences where
  toPath ListContactReferences' {..} =
    Prelude.mconcat
      [ "/contact/references/",
        Data.toBS instanceId,
        "/",
        Data.toBS contactId
      ]

instance Data.ToQuery ListContactReferences where
  toQuery ListContactReferences' {..} =
    Prelude.mconcat
      [ "nextToken" Data.=: nextToken,
        "referenceTypes"
          Data.=: Data.toQueryList "member" referenceTypes
      ]

-- | /See:/ 'newListContactReferencesResponse' smart constructor.
data ListContactReferencesResponse = ListContactReferencesResponse'
  { -- | If there are additional results, this is the token for the next set of
    -- results.
    --
    -- This is always returned as null in the response.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Information about the flows.
    referenceSummaryList :: Prelude.Maybe [ReferenceSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListContactReferencesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listContactReferencesResponse_nextToken' - If there are additional results, this is the token for the next set of
-- results.
--
-- This is always returned as null in the response.
--
-- 'referenceSummaryList', 'listContactReferencesResponse_referenceSummaryList' - Information about the flows.
--
-- 'httpStatus', 'listContactReferencesResponse_httpStatus' - The response's http status code.
newListContactReferencesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListContactReferencesResponse
newListContactReferencesResponse pHttpStatus_ =
  ListContactReferencesResponse'
    { nextToken =
        Prelude.Nothing,
      referenceSummaryList = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If there are additional results, this is the token for the next set of
-- results.
--
-- This is always returned as null in the response.
listContactReferencesResponse_nextToken :: Lens.Lens' ListContactReferencesResponse (Prelude.Maybe Prelude.Text)
listContactReferencesResponse_nextToken = Lens.lens (\ListContactReferencesResponse' {nextToken} -> nextToken) (\s@ListContactReferencesResponse' {} a -> s {nextToken = a} :: ListContactReferencesResponse)

-- | Information about the flows.
listContactReferencesResponse_referenceSummaryList :: Lens.Lens' ListContactReferencesResponse (Prelude.Maybe [ReferenceSummary])
listContactReferencesResponse_referenceSummaryList = Lens.lens (\ListContactReferencesResponse' {referenceSummaryList} -> referenceSummaryList) (\s@ListContactReferencesResponse' {} a -> s {referenceSummaryList = a} :: ListContactReferencesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listContactReferencesResponse_httpStatus :: Lens.Lens' ListContactReferencesResponse Prelude.Int
listContactReferencesResponse_httpStatus = Lens.lens (\ListContactReferencesResponse' {httpStatus} -> httpStatus) (\s@ListContactReferencesResponse' {} a -> s {httpStatus = a} :: ListContactReferencesResponse)

instance Prelude.NFData ListContactReferencesResponse where
  rnf ListContactReferencesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf referenceSummaryList
      `Prelude.seq` Prelude.rnf httpStatus
