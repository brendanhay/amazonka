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
-- Module      : Network.AWS.DirectoryService.DescribeTrusts
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.DirectoryService.DescribeTrusts
  ( -- * Creating a Request
    DescribeTrusts (..),
    newDescribeTrusts,

    -- * Request Lenses
    describeTrusts_nextToken,
    describeTrusts_directoryId,
    describeTrusts_limit,
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

import Network.AWS.DirectoryService.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Describes the trust relationships for a particular AWS Managed Microsoft
-- AD directory. If no input parameters are are provided, such as directory
-- ID or trust ID, this request describes all the trust relationships.
--
-- /See:/ 'newDescribeTrusts' smart constructor.
data DescribeTrusts = DescribeTrusts'
  { -- | The /DescribeTrustsResult.NextToken/ value from a previous call to
    -- DescribeTrusts. Pass null if this is the first call.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The Directory ID of the AWS directory that is a part of the requested
    -- trust relationship.
    directoryId :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of objects to return.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | A list of identifiers of the trust relationships for which to obtain the
    -- information. If this member is null, all trust relationships that belong
    -- to the current account are returned.
    --
    -- An empty list results in an @InvalidParameterException@ being thrown.
    trustIds :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DescribeTrusts' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeTrusts_nextToken' - The /DescribeTrustsResult.NextToken/ value from a previous call to
-- DescribeTrusts. Pass null if this is the first call.
--
-- 'directoryId', 'describeTrusts_directoryId' - The Directory ID of the AWS directory that is a part of the requested
-- trust relationship.
--
-- 'limit', 'describeTrusts_limit' - The maximum number of objects to return.
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
    { nextToken = Prelude.Nothing,
      directoryId = Prelude.Nothing,
      limit = Prelude.Nothing,
      trustIds = Prelude.Nothing
    }

-- | The /DescribeTrustsResult.NextToken/ value from a previous call to
-- DescribeTrusts. Pass null if this is the first call.
describeTrusts_nextToken :: Lens.Lens' DescribeTrusts (Prelude.Maybe Prelude.Text)
describeTrusts_nextToken = Lens.lens (\DescribeTrusts' {nextToken} -> nextToken) (\s@DescribeTrusts' {} a -> s {nextToken = a} :: DescribeTrusts)

-- | The Directory ID of the AWS directory that is a part of the requested
-- trust relationship.
describeTrusts_directoryId :: Lens.Lens' DescribeTrusts (Prelude.Maybe Prelude.Text)
describeTrusts_directoryId = Lens.lens (\DescribeTrusts' {directoryId} -> directoryId) (\s@DescribeTrusts' {} a -> s {directoryId = a} :: DescribeTrusts)

-- | The maximum number of objects to return.
describeTrusts_limit :: Lens.Lens' DescribeTrusts (Prelude.Maybe Prelude.Natural)
describeTrusts_limit = Lens.lens (\DescribeTrusts' {limit} -> limit) (\s@DescribeTrusts' {} a -> s {limit = a} :: DescribeTrusts)

-- | A list of identifiers of the trust relationships for which to obtain the
-- information. If this member is null, all trust relationships that belong
-- to the current account are returned.
--
-- An empty list results in an @InvalidParameterException@ being thrown.
describeTrusts_trustIds :: Lens.Lens' DescribeTrusts (Prelude.Maybe [Prelude.Text])
describeTrusts_trustIds = Lens.lens (\DescribeTrusts' {trustIds} -> trustIds) (\s@DescribeTrusts' {} a -> s {trustIds = a} :: DescribeTrusts) Prelude.. Lens.mapping Prelude._Coerce

instance Pager.AWSPager DescribeTrusts where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? describeTrustsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? describeTrustsResponse_trusts Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& describeTrusts_nextToken
          Lens..~ rs
          Lens.^? describeTrustsResponse_nextToken Prelude.. Lens._Just

instance Prelude.AWSRequest DescribeTrusts where
  type Rs DescribeTrusts = DescribeTrustsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeTrustsResponse'
            Prelude.<$> (x Prelude..?> "NextToken")
            Prelude.<*> (x Prelude..?> "Trusts" Prelude..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeTrusts

instance Prelude.NFData DescribeTrusts

instance Prelude.ToHeaders DescribeTrusts where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "DirectoryService_20150416.DescribeTrusts" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DescribeTrusts where
  toJSON DescribeTrusts' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("NextToken" Prelude..=) Prelude.<$> nextToken,
            ("DirectoryId" Prelude..=) Prelude.<$> directoryId,
            ("Limit" Prelude..=) Prelude.<$> limit,
            ("TrustIds" Prelude..=) Prelude.<$> trustIds
          ]
      )

instance Prelude.ToPath DescribeTrusts where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DescribeTrusts where
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
describeTrustsResponse_trusts = Lens.lens (\DescribeTrustsResponse' {trusts} -> trusts) (\s@DescribeTrustsResponse' {} a -> s {trusts = a} :: DescribeTrustsResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
describeTrustsResponse_httpStatus :: Lens.Lens' DescribeTrustsResponse Prelude.Int
describeTrustsResponse_httpStatus = Lens.lens (\DescribeTrustsResponse' {httpStatus} -> httpStatus) (\s@DescribeTrustsResponse' {} a -> s {httpStatus = a} :: DescribeTrustsResponse)

instance Prelude.NFData DescribeTrustsResponse
