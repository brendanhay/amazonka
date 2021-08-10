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
-- Module      : Network.AWS.SSM.DescribeInstanceAssociationsStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The status of the associations for the instance(s).
--
-- This operation returns paginated results.
module Network.AWS.SSM.DescribeInstanceAssociationsStatus
  ( -- * Creating a Request
    DescribeInstanceAssociationsStatus (..),
    newDescribeInstanceAssociationsStatus,

    -- * Request Lenses
    describeInstanceAssociationsStatus_nextToken,
    describeInstanceAssociationsStatus_maxResults,
    describeInstanceAssociationsStatus_instanceId,

    -- * Destructuring the Response
    DescribeInstanceAssociationsStatusResponse (..),
    newDescribeInstanceAssociationsStatusResponse,

    -- * Response Lenses
    describeInstanceAssociationsStatusResponse_nextToken,
    describeInstanceAssociationsStatusResponse_instanceAssociationStatusInfos,
    describeInstanceAssociationsStatusResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSM.Types

-- | /See:/ 'newDescribeInstanceAssociationsStatus' smart constructor.
data DescribeInstanceAssociationsStatus = DescribeInstanceAssociationsStatus'
  { -- | The token for the next set of items to return. (You received this token
    -- from a previous call.)
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of items to return for this call. The call also
    -- returns a token that you can specify in a subsequent call to get the
    -- next set of results.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The instance IDs for which you want association status information.
    instanceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeInstanceAssociationsStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeInstanceAssociationsStatus_nextToken' - The token for the next set of items to return. (You received this token
-- from a previous call.)
--
-- 'maxResults', 'describeInstanceAssociationsStatus_maxResults' - The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
--
-- 'instanceId', 'describeInstanceAssociationsStatus_instanceId' - The instance IDs for which you want association status information.
newDescribeInstanceAssociationsStatus ::
  -- | 'instanceId'
  Prelude.Text ->
  DescribeInstanceAssociationsStatus
newDescribeInstanceAssociationsStatus pInstanceId_ =
  DescribeInstanceAssociationsStatus'
    { nextToken =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      instanceId = pInstanceId_
    }

-- | The token for the next set of items to return. (You received this token
-- from a previous call.)
describeInstanceAssociationsStatus_nextToken :: Lens.Lens' DescribeInstanceAssociationsStatus (Prelude.Maybe Prelude.Text)
describeInstanceAssociationsStatus_nextToken = Lens.lens (\DescribeInstanceAssociationsStatus' {nextToken} -> nextToken) (\s@DescribeInstanceAssociationsStatus' {} a -> s {nextToken = a} :: DescribeInstanceAssociationsStatus)

-- | The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
describeInstanceAssociationsStatus_maxResults :: Lens.Lens' DescribeInstanceAssociationsStatus (Prelude.Maybe Prelude.Natural)
describeInstanceAssociationsStatus_maxResults = Lens.lens (\DescribeInstanceAssociationsStatus' {maxResults} -> maxResults) (\s@DescribeInstanceAssociationsStatus' {} a -> s {maxResults = a} :: DescribeInstanceAssociationsStatus)

-- | The instance IDs for which you want association status information.
describeInstanceAssociationsStatus_instanceId :: Lens.Lens' DescribeInstanceAssociationsStatus Prelude.Text
describeInstanceAssociationsStatus_instanceId = Lens.lens (\DescribeInstanceAssociationsStatus' {instanceId} -> instanceId) (\s@DescribeInstanceAssociationsStatus' {} a -> s {instanceId = a} :: DescribeInstanceAssociationsStatus)

instance
  Core.AWSPager
    DescribeInstanceAssociationsStatus
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeInstanceAssociationsStatusResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeInstanceAssociationsStatusResponse_instanceAssociationStatusInfos
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeInstanceAssociationsStatus_nextToken
          Lens..~ rs
          Lens.^? describeInstanceAssociationsStatusResponse_nextToken
            Prelude.. Lens._Just

instance
  Core.AWSRequest
    DescribeInstanceAssociationsStatus
  where
  type
    AWSResponse DescribeInstanceAssociationsStatus =
      DescribeInstanceAssociationsStatusResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeInstanceAssociationsStatusResponse'
            Prelude.<$> (x Core..?> "NextToken")
              Prelude.<*> ( x Core..?> "InstanceAssociationStatusInfos"
                              Core..!@ Prelude.mempty
                          )
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeInstanceAssociationsStatus

instance
  Prelude.NFData
    DescribeInstanceAssociationsStatus

instance
  Core.ToHeaders
    DescribeInstanceAssociationsStatus
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonSSM.DescribeInstanceAssociationsStatus" ::
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
    DescribeInstanceAssociationsStatus
  where
  toJSON DescribeInstanceAssociationsStatus' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            Prelude.Just ("InstanceId" Core..= instanceId)
          ]
      )

instance
  Core.ToPath
    DescribeInstanceAssociationsStatus
  where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    DescribeInstanceAssociationsStatus
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeInstanceAssociationsStatusResponse' smart constructor.
data DescribeInstanceAssociationsStatusResponse = DescribeInstanceAssociationsStatusResponse'
  { -- | The token to use when requesting the next set of items. If there are no
    -- additional items to return, the string is empty.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Status information about the association.
    instanceAssociationStatusInfos :: Prelude.Maybe [InstanceAssociationStatusInfo],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeInstanceAssociationsStatusResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeInstanceAssociationsStatusResponse_nextToken' - The token to use when requesting the next set of items. If there are no
-- additional items to return, the string is empty.
--
-- 'instanceAssociationStatusInfos', 'describeInstanceAssociationsStatusResponse_instanceAssociationStatusInfos' - Status information about the association.
--
-- 'httpStatus', 'describeInstanceAssociationsStatusResponse_httpStatus' - The response's http status code.
newDescribeInstanceAssociationsStatusResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeInstanceAssociationsStatusResponse
newDescribeInstanceAssociationsStatusResponse
  pHttpStatus_ =
    DescribeInstanceAssociationsStatusResponse'
      { nextToken =
          Prelude.Nothing,
        instanceAssociationStatusInfos =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The token to use when requesting the next set of items. If there are no
-- additional items to return, the string is empty.
describeInstanceAssociationsStatusResponse_nextToken :: Lens.Lens' DescribeInstanceAssociationsStatusResponse (Prelude.Maybe Prelude.Text)
describeInstanceAssociationsStatusResponse_nextToken = Lens.lens (\DescribeInstanceAssociationsStatusResponse' {nextToken} -> nextToken) (\s@DescribeInstanceAssociationsStatusResponse' {} a -> s {nextToken = a} :: DescribeInstanceAssociationsStatusResponse)

-- | Status information about the association.
describeInstanceAssociationsStatusResponse_instanceAssociationStatusInfos :: Lens.Lens' DescribeInstanceAssociationsStatusResponse (Prelude.Maybe [InstanceAssociationStatusInfo])
describeInstanceAssociationsStatusResponse_instanceAssociationStatusInfos = Lens.lens (\DescribeInstanceAssociationsStatusResponse' {instanceAssociationStatusInfos} -> instanceAssociationStatusInfos) (\s@DescribeInstanceAssociationsStatusResponse' {} a -> s {instanceAssociationStatusInfos = a} :: DescribeInstanceAssociationsStatusResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeInstanceAssociationsStatusResponse_httpStatus :: Lens.Lens' DescribeInstanceAssociationsStatusResponse Prelude.Int
describeInstanceAssociationsStatusResponse_httpStatus = Lens.lens (\DescribeInstanceAssociationsStatusResponse' {httpStatus} -> httpStatus) (\s@DescribeInstanceAssociationsStatusResponse' {} a -> s {httpStatus = a} :: DescribeInstanceAssociationsStatusResponse)

instance
  Prelude.NFData
    DescribeInstanceAssociationsStatusResponse
