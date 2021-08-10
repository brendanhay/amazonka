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
-- Module      : Network.AWS.SSM.DescribeEffectiveInstanceAssociations
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- All associations for the instance(s).
--
-- This operation returns paginated results.
module Network.AWS.SSM.DescribeEffectiveInstanceAssociations
  ( -- * Creating a Request
    DescribeEffectiveInstanceAssociations (..),
    newDescribeEffectiveInstanceAssociations,

    -- * Request Lenses
    describeEffectiveInstanceAssociations_nextToken,
    describeEffectiveInstanceAssociations_maxResults,
    describeEffectiveInstanceAssociations_instanceId,

    -- * Destructuring the Response
    DescribeEffectiveInstanceAssociationsResponse (..),
    newDescribeEffectiveInstanceAssociationsResponse,

    -- * Response Lenses
    describeEffectiveInstanceAssociationsResponse_nextToken,
    describeEffectiveInstanceAssociationsResponse_associations,
    describeEffectiveInstanceAssociationsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSM.Types

-- | /See:/ 'newDescribeEffectiveInstanceAssociations' smart constructor.
data DescribeEffectiveInstanceAssociations = DescribeEffectiveInstanceAssociations'
  { -- | The token for the next set of items to return. (You received this token
    -- from a previous call.)
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of items to return for this call. The call also
    -- returns a token that you can specify in a subsequent call to get the
    -- next set of results.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The instance ID for which you want to view all associations.
    instanceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeEffectiveInstanceAssociations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeEffectiveInstanceAssociations_nextToken' - The token for the next set of items to return. (You received this token
-- from a previous call.)
--
-- 'maxResults', 'describeEffectiveInstanceAssociations_maxResults' - The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
--
-- 'instanceId', 'describeEffectiveInstanceAssociations_instanceId' - The instance ID for which you want to view all associations.
newDescribeEffectiveInstanceAssociations ::
  -- | 'instanceId'
  Prelude.Text ->
  DescribeEffectiveInstanceAssociations
newDescribeEffectiveInstanceAssociations pInstanceId_ =
  DescribeEffectiveInstanceAssociations'
    { nextToken =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      instanceId = pInstanceId_
    }

-- | The token for the next set of items to return. (You received this token
-- from a previous call.)
describeEffectiveInstanceAssociations_nextToken :: Lens.Lens' DescribeEffectiveInstanceAssociations (Prelude.Maybe Prelude.Text)
describeEffectiveInstanceAssociations_nextToken = Lens.lens (\DescribeEffectiveInstanceAssociations' {nextToken} -> nextToken) (\s@DescribeEffectiveInstanceAssociations' {} a -> s {nextToken = a} :: DescribeEffectiveInstanceAssociations)

-- | The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
describeEffectiveInstanceAssociations_maxResults :: Lens.Lens' DescribeEffectiveInstanceAssociations (Prelude.Maybe Prelude.Natural)
describeEffectiveInstanceAssociations_maxResults = Lens.lens (\DescribeEffectiveInstanceAssociations' {maxResults} -> maxResults) (\s@DescribeEffectiveInstanceAssociations' {} a -> s {maxResults = a} :: DescribeEffectiveInstanceAssociations)

-- | The instance ID for which you want to view all associations.
describeEffectiveInstanceAssociations_instanceId :: Lens.Lens' DescribeEffectiveInstanceAssociations Prelude.Text
describeEffectiveInstanceAssociations_instanceId = Lens.lens (\DescribeEffectiveInstanceAssociations' {instanceId} -> instanceId) (\s@DescribeEffectiveInstanceAssociations' {} a -> s {instanceId = a} :: DescribeEffectiveInstanceAssociations)

instance
  Core.AWSPager
    DescribeEffectiveInstanceAssociations
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeEffectiveInstanceAssociationsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeEffectiveInstanceAssociationsResponse_associations
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeEffectiveInstanceAssociations_nextToken
          Lens..~ rs
            Lens.^? describeEffectiveInstanceAssociationsResponse_nextToken
              Prelude.. Lens._Just

instance
  Core.AWSRequest
    DescribeEffectiveInstanceAssociations
  where
  type
    AWSResponse
      DescribeEffectiveInstanceAssociations =
      DescribeEffectiveInstanceAssociationsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeEffectiveInstanceAssociationsResponse'
            Prelude.<$> (x Core..?> "NextToken")
              Prelude.<*> (x Core..?> "Associations" Core..!@ Prelude.mempty)
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeEffectiveInstanceAssociations

instance
  Prelude.NFData
    DescribeEffectiveInstanceAssociations

instance
  Core.ToHeaders
    DescribeEffectiveInstanceAssociations
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonSSM.DescribeEffectiveInstanceAssociations" ::
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
    DescribeEffectiveInstanceAssociations
  where
  toJSON DescribeEffectiveInstanceAssociations' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            Prelude.Just ("InstanceId" Core..= instanceId)
          ]
      )

instance
  Core.ToPath
    DescribeEffectiveInstanceAssociations
  where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    DescribeEffectiveInstanceAssociations
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeEffectiveInstanceAssociationsResponse' smart constructor.
data DescribeEffectiveInstanceAssociationsResponse = DescribeEffectiveInstanceAssociationsResponse'
  { -- | The token to use when requesting the next set of items. If there are no
    -- additional items to return, the string is empty.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The associations for the requested instance.
    associations :: Prelude.Maybe [InstanceAssociation],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeEffectiveInstanceAssociationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeEffectiveInstanceAssociationsResponse_nextToken' - The token to use when requesting the next set of items. If there are no
-- additional items to return, the string is empty.
--
-- 'associations', 'describeEffectiveInstanceAssociationsResponse_associations' - The associations for the requested instance.
--
-- 'httpStatus', 'describeEffectiveInstanceAssociationsResponse_httpStatus' - The response's http status code.
newDescribeEffectiveInstanceAssociationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeEffectiveInstanceAssociationsResponse
newDescribeEffectiveInstanceAssociationsResponse
  pHttpStatus_ =
    DescribeEffectiveInstanceAssociationsResponse'
      { nextToken =
          Prelude.Nothing,
        associations =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The token to use when requesting the next set of items. If there are no
-- additional items to return, the string is empty.
describeEffectiveInstanceAssociationsResponse_nextToken :: Lens.Lens' DescribeEffectiveInstanceAssociationsResponse (Prelude.Maybe Prelude.Text)
describeEffectiveInstanceAssociationsResponse_nextToken = Lens.lens (\DescribeEffectiveInstanceAssociationsResponse' {nextToken} -> nextToken) (\s@DescribeEffectiveInstanceAssociationsResponse' {} a -> s {nextToken = a} :: DescribeEffectiveInstanceAssociationsResponse)

-- | The associations for the requested instance.
describeEffectiveInstanceAssociationsResponse_associations :: Lens.Lens' DescribeEffectiveInstanceAssociationsResponse (Prelude.Maybe [InstanceAssociation])
describeEffectiveInstanceAssociationsResponse_associations = Lens.lens (\DescribeEffectiveInstanceAssociationsResponse' {associations} -> associations) (\s@DescribeEffectiveInstanceAssociationsResponse' {} a -> s {associations = a} :: DescribeEffectiveInstanceAssociationsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeEffectiveInstanceAssociationsResponse_httpStatus :: Lens.Lens' DescribeEffectiveInstanceAssociationsResponse Prelude.Int
describeEffectiveInstanceAssociationsResponse_httpStatus = Lens.lens (\DescribeEffectiveInstanceAssociationsResponse' {httpStatus} -> httpStatus) (\s@DescribeEffectiveInstanceAssociationsResponse' {} a -> s {httpStatus = a} :: DescribeEffectiveInstanceAssociationsResponse)

instance
  Prelude.NFData
    DescribeEffectiveInstanceAssociationsResponse
