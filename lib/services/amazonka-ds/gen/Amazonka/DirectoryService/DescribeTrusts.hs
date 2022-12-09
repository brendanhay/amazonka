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
-- Module      : Amazonka.DirectoryService.DescribeTrusts
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Obtains information about the trust relationships for this account.
--
-- If no input parameters are provided, such as DirectoryId or TrustIds,
-- this request describes all the trust relationships belonging to the
-- account.
--
-- This operation returns paginated results.
module Amazonka.DirectoryService.DescribeTrusts
  ( -- * Creating a Request
    DescribeTrusts (..),
    newDescribeTrusts,

    -- * Request Lenses
    describeTrusts_directoryId,
    describeTrusts_limit,
    describeTrusts_nextToken,
    describeTrusts_trustIds,

    -- * Destructuring the Response
    DescribeTrustsResponse (..),
    newDescribeTrustsResponse,

    -- * Response Lenses
    describeTrustsResponse_nextToken,
    describeTrustsResponse_trusts,
    describeTrustsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DirectoryService.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Describes the trust relationships for a particular Managed Microsoft AD
-- directory. If no input parameters are provided, such as directory ID or
-- trust ID, this request describes all the trust relationships.
--
-- /See:/ 'newDescribeTrusts' smart constructor.
data DescribeTrusts = DescribeTrusts'
  { -- | The Directory ID of the Amazon Web Services directory that is a part of
    -- the requested trust relationship.
    directoryId :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of objects to return.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | The /DescribeTrustsResult.NextToken/ value from a previous call to
    -- DescribeTrusts. Pass null if this is the first call.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of identifiers of the trust relationships for which to obtain the
    -- information. If this member is null, all trust relationships that belong
    -- to the current account are returned.
    --
    -- An empty list results in an @InvalidParameterException@ being thrown.
    trustIds :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeTrusts' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'directoryId', 'describeTrusts_directoryId' - The Directory ID of the Amazon Web Services directory that is a part of
-- the requested trust relationship.
--
-- 'limit', 'describeTrusts_limit' - The maximum number of objects to return.
--
-- 'nextToken', 'describeTrusts_nextToken' - The /DescribeTrustsResult.NextToken/ value from a previous call to
-- DescribeTrusts. Pass null if this is the first call.
--
-- 'trustIds', 'describeTrusts_trustIds' - A list of identifiers of the trust relationships for which to obtain the
-- information. If this member is null, all trust relationships that belong
-- to the current account are returned.
--
-- An empty list results in an @InvalidParameterException@ being thrown.
newDescribeTrusts ::
  DescribeTrusts
newDescribeTrusts =
  DescribeTrusts'
    { directoryId = Prelude.Nothing,
      limit = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      trustIds = Prelude.Nothing
    }

-- | The Directory ID of the Amazon Web Services directory that is a part of
-- the requested trust relationship.
describeTrusts_directoryId :: Lens.Lens' DescribeTrusts (Prelude.Maybe Prelude.Text)
describeTrusts_directoryId = Lens.lens (\DescribeTrusts' {directoryId} -> directoryId) (\s@DescribeTrusts' {} a -> s {directoryId = a} :: DescribeTrusts)

-- | The maximum number of objects to return.
describeTrusts_limit :: Lens.Lens' DescribeTrusts (Prelude.Maybe Prelude.Natural)
describeTrusts_limit = Lens.lens (\DescribeTrusts' {limit} -> limit) (\s@DescribeTrusts' {} a -> s {limit = a} :: DescribeTrusts)

-- | The /DescribeTrustsResult.NextToken/ value from a previous call to
-- DescribeTrusts. Pass null if this is the first call.
describeTrusts_nextToken :: Lens.Lens' DescribeTrusts (Prelude.Maybe Prelude.Text)
describeTrusts_nextToken = Lens.lens (\DescribeTrusts' {nextToken} -> nextToken) (\s@DescribeTrusts' {} a -> s {nextToken = a} :: DescribeTrusts)

-- | A list of identifiers of the trust relationships for which to obtain the
-- information. If this member is null, all trust relationships that belong
-- to the current account are returned.
--
-- An empty list results in an @InvalidParameterException@ being thrown.
describeTrusts_trustIds :: Lens.Lens' DescribeTrusts (Prelude.Maybe [Prelude.Text])
describeTrusts_trustIds = Lens.lens (\DescribeTrusts' {trustIds} -> trustIds) (\s@DescribeTrusts' {} a -> s {trustIds = a} :: DescribeTrusts) Prelude.. Lens.mapping Lens.coerced

instance Core.AWSPager DescribeTrusts where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeTrustsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeTrustsResponse_trusts Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeTrusts_nextToken
          Lens..~ rs
          Lens.^? describeTrustsResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest DescribeTrusts where
  type
    AWSResponse DescribeTrusts =
      DescribeTrustsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeTrustsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "Trusts" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeTrusts where
  hashWithSalt _salt DescribeTrusts' {..} =
    _salt `Prelude.hashWithSalt` directoryId
      `Prelude.hashWithSalt` limit
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` trustIds

instance Prelude.NFData DescribeTrusts where
  rnf DescribeTrusts' {..} =
    Prelude.rnf directoryId
      `Prelude.seq` Prelude.rnf limit
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf trustIds

instance Data.ToHeaders DescribeTrusts where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "DirectoryService_20150416.DescribeTrusts" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeTrusts where
  toJSON DescribeTrusts' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DirectoryId" Data..=) Prelude.<$> directoryId,
            ("Limit" Data..=) Prelude.<$> limit,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("TrustIds" Data..=) Prelude.<$> trustIds
          ]
      )

instance Data.ToPath DescribeTrusts where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeTrusts where
  toQuery = Prelude.const Prelude.mempty

-- | The result of a DescribeTrust request.
--
-- /See:/ 'newDescribeTrustsResponse' smart constructor.
data DescribeTrustsResponse = DescribeTrustsResponse'
  { -- | If not null, more results are available. Pass this value for the
    -- /NextToken/ parameter in a subsequent call to DescribeTrusts to retrieve
    -- the next set of items.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The list of Trust objects that were retrieved.
    --
    -- It is possible that this list contains less than the number of items
    -- specified in the /Limit/ member of the request. This occurs if there are
    -- less than the requested number of items left to retrieve, or if the
    -- limitations of the operation have been exceeded.
    trusts :: Prelude.Maybe [Trust],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeTrustsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeTrustsResponse_nextToken' - If not null, more results are available. Pass this value for the
-- /NextToken/ parameter in a subsequent call to DescribeTrusts to retrieve
-- the next set of items.
--
-- 'trusts', 'describeTrustsResponse_trusts' - The list of Trust objects that were retrieved.
--
-- It is possible that this list contains less than the number of items
-- specified in the /Limit/ member of the request. This occurs if there are
-- less than the requested number of items left to retrieve, or if the
-- limitations of the operation have been exceeded.
--
-- 'httpStatus', 'describeTrustsResponse_httpStatus' - The response's http status code.
newDescribeTrustsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeTrustsResponse
newDescribeTrustsResponse pHttpStatus_ =
  DescribeTrustsResponse'
    { nextToken =
        Prelude.Nothing,
      trusts = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If not null, more results are available. Pass this value for the
-- /NextToken/ parameter in a subsequent call to DescribeTrusts to retrieve
-- the next set of items.
describeTrustsResponse_nextToken :: Lens.Lens' DescribeTrustsResponse (Prelude.Maybe Prelude.Text)
describeTrustsResponse_nextToken = Lens.lens (\DescribeTrustsResponse' {nextToken} -> nextToken) (\s@DescribeTrustsResponse' {} a -> s {nextToken = a} :: DescribeTrustsResponse)

-- | The list of Trust objects that were retrieved.
--
-- It is possible that this list contains less than the number of items
-- specified in the /Limit/ member of the request. This occurs if there are
-- less than the requested number of items left to retrieve, or if the
-- limitations of the operation have been exceeded.
describeTrustsResponse_trusts :: Lens.Lens' DescribeTrustsResponse (Prelude.Maybe [Trust])
describeTrustsResponse_trusts = Lens.lens (\DescribeTrustsResponse' {trusts} -> trusts) (\s@DescribeTrustsResponse' {} a -> s {trusts = a} :: DescribeTrustsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeTrustsResponse_httpStatus :: Lens.Lens' DescribeTrustsResponse Prelude.Int
describeTrustsResponse_httpStatus = Lens.lens (\DescribeTrustsResponse' {httpStatus} -> httpStatus) (\s@DescribeTrustsResponse' {} a -> s {httpStatus = a} :: DescribeTrustsResponse)

instance Prelude.NFData DescribeTrustsResponse where
  rnf DescribeTrustsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf trusts
      `Prelude.seq` Prelude.rnf httpStatus
