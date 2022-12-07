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
-- Module      : Amazonka.SSM.DescribeInstanceInformation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of your managed nodes, including information about
-- the operating system platform, the version of SSM Agent installed on the
-- managed node, node status, and so on.
--
-- If you specify one or more managed node IDs, it returns information for
-- those managed nodes. If you don\'t specify node IDs, it returns
-- information for all your managed nodes. If you specify a node ID that
-- isn\'t valid or a node that you don\'t own, you receive an error.
--
-- The @IamRole@ field for this API operation is the Identity and Access
-- Management (IAM) role assigned to on-premises managed nodes. This call
-- doesn\'t return the IAM role for EC2 instances.
--
-- This operation returns paginated results.
module Amazonka.SSM.DescribeInstanceInformation
  ( -- * Creating a Request
    DescribeInstanceInformation (..),
    newDescribeInstanceInformation,

    -- * Request Lenses
    describeInstanceInformation_nextToken,
    describeInstanceInformation_filters,
    describeInstanceInformation_maxResults,
    describeInstanceInformation_instanceInformationFilterList,

    -- * Destructuring the Response
    DescribeInstanceInformationResponse (..),
    newDescribeInstanceInformationResponse,

    -- * Response Lenses
    describeInstanceInformationResponse_nextToken,
    describeInstanceInformationResponse_instanceInformationList,
    describeInstanceInformationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSM.Types

-- | /See:/ 'newDescribeInstanceInformation' smart constructor.
data DescribeInstanceInformation = DescribeInstanceInformation'
  { -- | The token for the next set of items to return. (You received this token
    -- from a previous call.)
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | One or more filters. Use a filter to return a more specific list of
    -- managed nodes. You can filter based on tags applied to your managed
    -- nodes. Use this @Filters@ data type instead of
    -- @InstanceInformationFilterList@, which is deprecated.
    filters :: Prelude.Maybe [InstanceInformationStringFilter],
    -- | The maximum number of items to return for this call. The call also
    -- returns a token that you can specify in a subsequent call to get the
    -- next set of results.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | This is a legacy method. We recommend that you don\'t use this method.
    -- Instead, use the @Filters@ data type. @Filters@ enables you to return
    -- node information by filtering based on tags applied to managed nodes.
    --
    -- Attempting to use @InstanceInformationFilterList@ and @Filters@ leads to
    -- an exception error.
    instanceInformationFilterList :: Prelude.Maybe [InstanceInformationFilter]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeInstanceInformation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeInstanceInformation_nextToken' - The token for the next set of items to return. (You received this token
-- from a previous call.)
--
-- 'filters', 'describeInstanceInformation_filters' - One or more filters. Use a filter to return a more specific list of
-- managed nodes. You can filter based on tags applied to your managed
-- nodes. Use this @Filters@ data type instead of
-- @InstanceInformationFilterList@, which is deprecated.
--
-- 'maxResults', 'describeInstanceInformation_maxResults' - The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
--
-- 'instanceInformationFilterList', 'describeInstanceInformation_instanceInformationFilterList' - This is a legacy method. We recommend that you don\'t use this method.
-- Instead, use the @Filters@ data type. @Filters@ enables you to return
-- node information by filtering based on tags applied to managed nodes.
--
-- Attempting to use @InstanceInformationFilterList@ and @Filters@ leads to
-- an exception error.
newDescribeInstanceInformation ::
  DescribeInstanceInformation
newDescribeInstanceInformation =
  DescribeInstanceInformation'
    { nextToken =
        Prelude.Nothing,
      filters = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      instanceInformationFilterList =
        Prelude.Nothing
    }

-- | The token for the next set of items to return. (You received this token
-- from a previous call.)
describeInstanceInformation_nextToken :: Lens.Lens' DescribeInstanceInformation (Prelude.Maybe Prelude.Text)
describeInstanceInformation_nextToken = Lens.lens (\DescribeInstanceInformation' {nextToken} -> nextToken) (\s@DescribeInstanceInformation' {} a -> s {nextToken = a} :: DescribeInstanceInformation)

-- | One or more filters. Use a filter to return a more specific list of
-- managed nodes. You can filter based on tags applied to your managed
-- nodes. Use this @Filters@ data type instead of
-- @InstanceInformationFilterList@, which is deprecated.
describeInstanceInformation_filters :: Lens.Lens' DescribeInstanceInformation (Prelude.Maybe [InstanceInformationStringFilter])
describeInstanceInformation_filters = Lens.lens (\DescribeInstanceInformation' {filters} -> filters) (\s@DescribeInstanceInformation' {} a -> s {filters = a} :: DescribeInstanceInformation) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
describeInstanceInformation_maxResults :: Lens.Lens' DescribeInstanceInformation (Prelude.Maybe Prelude.Natural)
describeInstanceInformation_maxResults = Lens.lens (\DescribeInstanceInformation' {maxResults} -> maxResults) (\s@DescribeInstanceInformation' {} a -> s {maxResults = a} :: DescribeInstanceInformation)

-- | This is a legacy method. We recommend that you don\'t use this method.
-- Instead, use the @Filters@ data type. @Filters@ enables you to return
-- node information by filtering based on tags applied to managed nodes.
--
-- Attempting to use @InstanceInformationFilterList@ and @Filters@ leads to
-- an exception error.
describeInstanceInformation_instanceInformationFilterList :: Lens.Lens' DescribeInstanceInformation (Prelude.Maybe [InstanceInformationFilter])
describeInstanceInformation_instanceInformationFilterList = Lens.lens (\DescribeInstanceInformation' {instanceInformationFilterList} -> instanceInformationFilterList) (\s@DescribeInstanceInformation' {} a -> s {instanceInformationFilterList = a} :: DescribeInstanceInformation) Prelude.. Lens.mapping Lens.coerced

instance Core.AWSPager DescribeInstanceInformation where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeInstanceInformationResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeInstanceInformationResponse_instanceInformationList
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeInstanceInformation_nextToken
          Lens..~ rs
          Lens.^? describeInstanceInformationResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeInstanceInformation where
  type
    AWSResponse DescribeInstanceInformation =
      DescribeInstanceInformationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeInstanceInformationResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> ( x Data..?> "InstanceInformationList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeInstanceInformation where
  hashWithSalt _salt DescribeInstanceInformation' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` instanceInformationFilterList

instance Prelude.NFData DescribeInstanceInformation where
  rnf DescribeInstanceInformation' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf instanceInformationFilterList

instance Data.ToHeaders DescribeInstanceInformation where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonSSM.DescribeInstanceInformation" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeInstanceInformation where
  toJSON DescribeInstanceInformation' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("NextToken" Data..=) Prelude.<$> nextToken,
            ("Filters" Data..=) Prelude.<$> filters,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("InstanceInformationFilterList" Data..=)
              Prelude.<$> instanceInformationFilterList
          ]
      )

instance Data.ToPath DescribeInstanceInformation where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeInstanceInformation where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeInstanceInformationResponse' smart constructor.
data DescribeInstanceInformationResponse = DescribeInstanceInformationResponse'
  { -- | The token to use when requesting the next set of items. If there are no
    -- additional items to return, the string is empty.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The managed node information list.
    instanceInformationList :: Prelude.Maybe [InstanceInformation],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeInstanceInformationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeInstanceInformationResponse_nextToken' - The token to use when requesting the next set of items. If there are no
-- additional items to return, the string is empty.
--
-- 'instanceInformationList', 'describeInstanceInformationResponse_instanceInformationList' - The managed node information list.
--
-- 'httpStatus', 'describeInstanceInformationResponse_httpStatus' - The response's http status code.
newDescribeInstanceInformationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeInstanceInformationResponse
newDescribeInstanceInformationResponse pHttpStatus_ =
  DescribeInstanceInformationResponse'
    { nextToken =
        Prelude.Nothing,
      instanceInformationList =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use when requesting the next set of items. If there are no
-- additional items to return, the string is empty.
describeInstanceInformationResponse_nextToken :: Lens.Lens' DescribeInstanceInformationResponse (Prelude.Maybe Prelude.Text)
describeInstanceInformationResponse_nextToken = Lens.lens (\DescribeInstanceInformationResponse' {nextToken} -> nextToken) (\s@DescribeInstanceInformationResponse' {} a -> s {nextToken = a} :: DescribeInstanceInformationResponse)

-- | The managed node information list.
describeInstanceInformationResponse_instanceInformationList :: Lens.Lens' DescribeInstanceInformationResponse (Prelude.Maybe [InstanceInformation])
describeInstanceInformationResponse_instanceInformationList = Lens.lens (\DescribeInstanceInformationResponse' {instanceInformationList} -> instanceInformationList) (\s@DescribeInstanceInformationResponse' {} a -> s {instanceInformationList = a} :: DescribeInstanceInformationResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeInstanceInformationResponse_httpStatus :: Lens.Lens' DescribeInstanceInformationResponse Prelude.Int
describeInstanceInformationResponse_httpStatus = Lens.lens (\DescribeInstanceInformationResponse' {httpStatus} -> httpStatus) (\s@DescribeInstanceInformationResponse' {} a -> s {httpStatus = a} :: DescribeInstanceInformationResponse)

instance
  Prelude.NFData
    DescribeInstanceInformationResponse
  where
  rnf DescribeInstanceInformationResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf instanceInformationList
      `Prelude.seq` Prelude.rnf httpStatus
