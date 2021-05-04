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
-- Module      : Network.AWS.SSM.DescribeInstanceInformation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of your instances, including information about the
-- operating system platform, the version of SSM Agent installed on the
-- instance, instance status, and so on.
--
-- If you specify one or more instance IDs, it returns information for
-- those instances. If you do not specify instance IDs, it returns
-- information for all your instances. If you specify an instance ID that
-- is not valid or an instance that you do not own, you receive an error.
--
-- The IamRole field for this API action is the Amazon Identity and Access
-- Management (IAM) role assigned to on-premises instances. This call does
-- not return the IAM role for EC2 instances.
--
-- This operation returns paginated results.
module Network.AWS.SSM.DescribeInstanceInformation
  ( -- * Creating a Request
    DescribeInstanceInformation (..),
    newDescribeInstanceInformation,

    -- * Request Lenses
    describeInstanceInformation_nextToken,
    describeInstanceInformation_maxResults,
    describeInstanceInformation_instanceInformationFilterList,
    describeInstanceInformation_filters,

    -- * Destructuring the Response
    DescribeInstanceInformationResponse (..),
    newDescribeInstanceInformationResponse,

    -- * Response Lenses
    describeInstanceInformationResponse_instanceInformationList,
    describeInstanceInformationResponse_nextToken,
    describeInstanceInformationResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSM.Types

-- | /See:/ 'newDescribeInstanceInformation' smart constructor.
data DescribeInstanceInformation = DescribeInstanceInformation'
  { -- | The token for the next set of items to return. (You received this token
    -- from a previous call.)
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of items to return for this call. The call also
    -- returns a token that you can specify in a subsequent call to get the
    -- next set of results.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | This is a legacy method. We recommend that you don\'t use this method.
    -- Instead, use the @Filters@ data type. @Filters@ enables you to return
    -- instance information by filtering based on tags applied to managed
    -- instances.
    --
    -- Attempting to use @InstanceInformationFilterList@ and @Filters@ leads to
    -- an exception error.
    instanceInformationFilterList :: Prelude.Maybe [InstanceInformationFilter],
    -- | One or more filters. Use a filter to return a more specific list of
    -- instances. You can filter based on tags applied to EC2 instances. Use
    -- this @Filters@ data type instead of @InstanceInformationFilterList@,
    -- which is deprecated.
    filters :: Prelude.Maybe [InstanceInformationStringFilter]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
-- 'maxResults', 'describeInstanceInformation_maxResults' - The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
--
-- 'instanceInformationFilterList', 'describeInstanceInformation_instanceInformationFilterList' - This is a legacy method. We recommend that you don\'t use this method.
-- Instead, use the @Filters@ data type. @Filters@ enables you to return
-- instance information by filtering based on tags applied to managed
-- instances.
--
-- Attempting to use @InstanceInformationFilterList@ and @Filters@ leads to
-- an exception error.
--
-- 'filters', 'describeInstanceInformation_filters' - One or more filters. Use a filter to return a more specific list of
-- instances. You can filter based on tags applied to EC2 instances. Use
-- this @Filters@ data type instead of @InstanceInformationFilterList@,
-- which is deprecated.
newDescribeInstanceInformation ::
  DescribeInstanceInformation
newDescribeInstanceInformation =
  DescribeInstanceInformation'
    { nextToken =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      instanceInformationFilterList =
        Prelude.Nothing,
      filters = Prelude.Nothing
    }

-- | The token for the next set of items to return. (You received this token
-- from a previous call.)
describeInstanceInformation_nextToken :: Lens.Lens' DescribeInstanceInformation (Prelude.Maybe Prelude.Text)
describeInstanceInformation_nextToken = Lens.lens (\DescribeInstanceInformation' {nextToken} -> nextToken) (\s@DescribeInstanceInformation' {} a -> s {nextToken = a} :: DescribeInstanceInformation)

-- | The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
describeInstanceInformation_maxResults :: Lens.Lens' DescribeInstanceInformation (Prelude.Maybe Prelude.Natural)
describeInstanceInformation_maxResults = Lens.lens (\DescribeInstanceInformation' {maxResults} -> maxResults) (\s@DescribeInstanceInformation' {} a -> s {maxResults = a} :: DescribeInstanceInformation)

-- | This is a legacy method. We recommend that you don\'t use this method.
-- Instead, use the @Filters@ data type. @Filters@ enables you to return
-- instance information by filtering based on tags applied to managed
-- instances.
--
-- Attempting to use @InstanceInformationFilterList@ and @Filters@ leads to
-- an exception error.
describeInstanceInformation_instanceInformationFilterList :: Lens.Lens' DescribeInstanceInformation (Prelude.Maybe [InstanceInformationFilter])
describeInstanceInformation_instanceInformationFilterList = Lens.lens (\DescribeInstanceInformation' {instanceInformationFilterList} -> instanceInformationFilterList) (\s@DescribeInstanceInformation' {} a -> s {instanceInformationFilterList = a} :: DescribeInstanceInformation) Prelude.. Lens.mapping Prelude._Coerce

-- | One or more filters. Use a filter to return a more specific list of
-- instances. You can filter based on tags applied to EC2 instances. Use
-- this @Filters@ data type instead of @InstanceInformationFilterList@,
-- which is deprecated.
describeInstanceInformation_filters :: Lens.Lens' DescribeInstanceInformation (Prelude.Maybe [InstanceInformationStringFilter])
describeInstanceInformation_filters = Lens.lens (\DescribeInstanceInformation' {filters} -> filters) (\s@DescribeInstanceInformation' {} a -> s {filters = a} :: DescribeInstanceInformation) Prelude.. Lens.mapping Prelude._Coerce

instance Pager.AWSPager DescribeInstanceInformation where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? describeInstanceInformationResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? describeInstanceInformationResponse_instanceInformationList
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& describeInstanceInformation_nextToken
          Lens..~ rs
          Lens.^? describeInstanceInformationResponse_nextToken
            Prelude.. Lens._Just

instance
  Prelude.AWSRequest
    DescribeInstanceInformation
  where
  type
    Rs DescribeInstanceInformation =
      DescribeInstanceInformationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeInstanceInformationResponse'
            Prelude.<$> ( x Prelude..?> "InstanceInformationList"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (x Prelude..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeInstanceInformation

instance Prelude.NFData DescribeInstanceInformation

instance
  Prelude.ToHeaders
    DescribeInstanceInformation
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AmazonSSM.DescribeInstanceInformation" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DescribeInstanceInformation where
  toJSON DescribeInstanceInformation' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("NextToken" Prelude..=) Prelude.<$> nextToken,
            ("MaxResults" Prelude..=) Prelude.<$> maxResults,
            ("InstanceInformationFilterList" Prelude..=)
              Prelude.<$> instanceInformationFilterList,
            ("Filters" Prelude..=) Prelude.<$> filters
          ]
      )

instance Prelude.ToPath DescribeInstanceInformation where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DescribeInstanceInformation where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeInstanceInformationResponse' smart constructor.
data DescribeInstanceInformationResponse = DescribeInstanceInformationResponse'
  { -- | The instance information list.
    instanceInformationList :: Prelude.Maybe [InstanceInformation],
    -- | The token to use when requesting the next set of items. If there are no
    -- additional items to return, the string is empty.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DescribeInstanceInformationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceInformationList', 'describeInstanceInformationResponse_instanceInformationList' - The instance information list.
--
-- 'nextToken', 'describeInstanceInformationResponse_nextToken' - The token to use when requesting the next set of items. If there are no
-- additional items to return, the string is empty.
--
-- 'httpStatus', 'describeInstanceInformationResponse_httpStatus' - The response's http status code.
newDescribeInstanceInformationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeInstanceInformationResponse
newDescribeInstanceInformationResponse pHttpStatus_ =
  DescribeInstanceInformationResponse'
    { instanceInformationList =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The instance information list.
describeInstanceInformationResponse_instanceInformationList :: Lens.Lens' DescribeInstanceInformationResponse (Prelude.Maybe [InstanceInformation])
describeInstanceInformationResponse_instanceInformationList = Lens.lens (\DescribeInstanceInformationResponse' {instanceInformationList} -> instanceInformationList) (\s@DescribeInstanceInformationResponse' {} a -> s {instanceInformationList = a} :: DescribeInstanceInformationResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The token to use when requesting the next set of items. If there are no
-- additional items to return, the string is empty.
describeInstanceInformationResponse_nextToken :: Lens.Lens' DescribeInstanceInformationResponse (Prelude.Maybe Prelude.Text)
describeInstanceInformationResponse_nextToken = Lens.lens (\DescribeInstanceInformationResponse' {nextToken} -> nextToken) (\s@DescribeInstanceInformationResponse' {} a -> s {nextToken = a} :: DescribeInstanceInformationResponse)

-- | The response's http status code.
describeInstanceInformationResponse_httpStatus :: Lens.Lens' DescribeInstanceInformationResponse Prelude.Int
describeInstanceInformationResponse_httpStatus = Lens.lens (\DescribeInstanceInformationResponse' {httpStatus} -> httpStatus) (\s@DescribeInstanceInformationResponse' {} a -> s {httpStatus = a} :: DescribeInstanceInformationResponse)

instance
  Prelude.NFData
    DescribeInstanceInformationResponse
