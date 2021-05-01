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
-- Module      : Network.AWS.EC2.DescribeAggregateIdFormat
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the longer ID format settings for all resource types in a
-- specific Region. This request is useful for performing a quick audit to
-- determine whether a specific Region is fully opted in for longer IDs
-- (17-character IDs).
--
-- This request only returns information about resource types that support
-- longer IDs.
--
-- The following resource types support longer IDs: @bundle@ |
-- @conversion-task@ | @customer-gateway@ | @dhcp-options@ |
-- @elastic-ip-allocation@ | @elastic-ip-association@ | @export-task@ |
-- @flow-log@ | @image@ | @import-task@ | @instance@ | @internet-gateway@ |
-- @network-acl@ | @network-acl-association@ | @network-interface@ |
-- @network-interface-attachment@ | @prefix-list@ | @reservation@ |
-- @route-table@ | @route-table-association@ | @security-group@ |
-- @snapshot@ | @subnet@ | @subnet-cidr-block-association@ | @volume@ |
-- @vpc@ | @vpc-cidr-block-association@ | @vpc-endpoint@ |
-- @vpc-peering-connection@ | @vpn-connection@ | @vpn-gateway@.
module Network.AWS.EC2.DescribeAggregateIdFormat
  ( -- * Creating a Request
    DescribeAggregateIdFormat (..),
    newDescribeAggregateIdFormat,

    -- * Request Lenses
    describeAggregateIdFormat_dryRun,

    -- * Destructuring the Response
    DescribeAggregateIdFormatResponse (..),
    newDescribeAggregateIdFormatResponse,

    -- * Response Lenses
    describeAggregateIdFormatResponse_useLongIdsAggregated,
    describeAggregateIdFormatResponse_statuses,
    describeAggregateIdFormatResponse_httpStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeAggregateIdFormat' smart constructor.
data DescribeAggregateIdFormat = DescribeAggregateIdFormat'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DescribeAggregateIdFormat' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'describeAggregateIdFormat_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
newDescribeAggregateIdFormat ::
  DescribeAggregateIdFormat
newDescribeAggregateIdFormat =
  DescribeAggregateIdFormat'
    { dryRun =
        Prelude.Nothing
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeAggregateIdFormat_dryRun :: Lens.Lens' DescribeAggregateIdFormat (Prelude.Maybe Prelude.Bool)
describeAggregateIdFormat_dryRun = Lens.lens (\DescribeAggregateIdFormat' {dryRun} -> dryRun) (\s@DescribeAggregateIdFormat' {} a -> s {dryRun = a} :: DescribeAggregateIdFormat)

instance Prelude.AWSRequest DescribeAggregateIdFormat where
  type
    Rs DescribeAggregateIdFormat =
      DescribeAggregateIdFormatResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeAggregateIdFormatResponse'
            Prelude.<$> (x Prelude..@? "useLongIdsAggregated")
            Prelude.<*> ( x Prelude..@? "statusSet" Prelude..!@ Prelude.mempty
                            Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeAggregateIdFormat

instance Prelude.NFData DescribeAggregateIdFormat

instance Prelude.ToHeaders DescribeAggregateIdFormat where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DescribeAggregateIdFormat where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DescribeAggregateIdFormat where
  toQuery DescribeAggregateIdFormat' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("DescribeAggregateIdFormat" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Prelude.=: dryRun
      ]

-- | /See:/ 'newDescribeAggregateIdFormatResponse' smart constructor.
data DescribeAggregateIdFormatResponse = DescribeAggregateIdFormatResponse'
  { -- | Indicates whether all resource types in the Region are configured to use
    -- longer IDs. This value is only @true@ if all users are configured to use
    -- longer IDs for all resources types in the Region.
    useLongIdsAggregated :: Prelude.Maybe Prelude.Bool,
    -- | Information about each resource\'s ID format.
    statuses :: Prelude.Maybe [IdFormat],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DescribeAggregateIdFormatResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'useLongIdsAggregated', 'describeAggregateIdFormatResponse_useLongIdsAggregated' - Indicates whether all resource types in the Region are configured to use
-- longer IDs. This value is only @true@ if all users are configured to use
-- longer IDs for all resources types in the Region.
--
-- 'statuses', 'describeAggregateIdFormatResponse_statuses' - Information about each resource\'s ID format.
--
-- 'httpStatus', 'describeAggregateIdFormatResponse_httpStatus' - The response's http status code.
newDescribeAggregateIdFormatResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeAggregateIdFormatResponse
newDescribeAggregateIdFormatResponse pHttpStatus_ =
  DescribeAggregateIdFormatResponse'
    { useLongIdsAggregated =
        Prelude.Nothing,
      statuses = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Indicates whether all resource types in the Region are configured to use
-- longer IDs. This value is only @true@ if all users are configured to use
-- longer IDs for all resources types in the Region.
describeAggregateIdFormatResponse_useLongIdsAggregated :: Lens.Lens' DescribeAggregateIdFormatResponse (Prelude.Maybe Prelude.Bool)
describeAggregateIdFormatResponse_useLongIdsAggregated = Lens.lens (\DescribeAggregateIdFormatResponse' {useLongIdsAggregated} -> useLongIdsAggregated) (\s@DescribeAggregateIdFormatResponse' {} a -> s {useLongIdsAggregated = a} :: DescribeAggregateIdFormatResponse)

-- | Information about each resource\'s ID format.
describeAggregateIdFormatResponse_statuses :: Lens.Lens' DescribeAggregateIdFormatResponse (Prelude.Maybe [IdFormat])
describeAggregateIdFormatResponse_statuses = Lens.lens (\DescribeAggregateIdFormatResponse' {statuses} -> statuses) (\s@DescribeAggregateIdFormatResponse' {} a -> s {statuses = a} :: DescribeAggregateIdFormatResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
describeAggregateIdFormatResponse_httpStatus :: Lens.Lens' DescribeAggregateIdFormatResponse Prelude.Int
describeAggregateIdFormatResponse_httpStatus = Lens.lens (\DescribeAggregateIdFormatResponse' {httpStatus} -> httpStatus) (\s@DescribeAggregateIdFormatResponse' {} a -> s {httpStatus = a} :: DescribeAggregateIdFormatResponse)

instance
  Prelude.NFData
    DescribeAggregateIdFormatResponse
