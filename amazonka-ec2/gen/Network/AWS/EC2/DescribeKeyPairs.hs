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
-- Module      : Network.AWS.EC2.DescribeKeyPairs
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified key pairs or all of your key pairs.
--
-- For more information about key pairs, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-key-pairs.html Key Pairs>
-- in the /Amazon Elastic Compute Cloud User Guide/.
module Network.AWS.EC2.DescribeKeyPairs
  ( -- * Creating a Request
    DescribeKeyPairs (..),
    newDescribeKeyPairs,

    -- * Request Lenses
    describeKeyPairs_dryRun,
    describeKeyPairs_keyPairIds,
    describeKeyPairs_filters,
    describeKeyPairs_keyNames,

    -- * Destructuring the Response
    DescribeKeyPairsResponse (..),
    newDescribeKeyPairsResponse,

    -- * Response Lenses
    describeKeyPairsResponse_keyPairs,
    describeKeyPairsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeKeyPairs' smart constructor.
data DescribeKeyPairs = DescribeKeyPairs'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | The IDs of the key pairs.
    keyPairIds :: Core.Maybe [Core.Text],
    -- | The filters.
    --
    -- -   @key-pair-id@ - The ID of the key pair.
    --
    -- -   @fingerprint@ - The fingerprint of the key pair.
    --
    -- -   @key-name@ - The name of the key pair.
    --
    -- -   @tag-key@ - The key of a tag assigned to the resource. Use this
    --     filter to find all resources assigned a tag with a specific key,
    --     regardless of the tag value.
    --
    -- -   @tag@:\<key> - The key\/value combination of a tag assigned to the
    --     resource. Use the tag key in the filter name and the tag value as
    --     the filter value. For example, to find all resources that have a tag
    --     with the key @Owner@ and the value @TeamA@, specify @tag:Owner@ for
    --     the filter name and @TeamA@ for the filter value.
    filters :: Core.Maybe [Filter],
    -- | The key pair names.
    --
    -- Default: Describes all your key pairs.
    keyNames :: Core.Maybe [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeKeyPairs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'describeKeyPairs_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'keyPairIds', 'describeKeyPairs_keyPairIds' - The IDs of the key pairs.
--
-- 'filters', 'describeKeyPairs_filters' - The filters.
--
-- -   @key-pair-id@ - The ID of the key pair.
--
-- -   @fingerprint@ - The fingerprint of the key pair.
--
-- -   @key-name@ - The name of the key pair.
--
-- -   @tag-key@ - The key of a tag assigned to the resource. Use this
--     filter to find all resources assigned a tag with a specific key,
--     regardless of the tag value.
--
-- -   @tag@:\<key> - The key\/value combination of a tag assigned to the
--     resource. Use the tag key in the filter name and the tag value as
--     the filter value. For example, to find all resources that have a tag
--     with the key @Owner@ and the value @TeamA@, specify @tag:Owner@ for
--     the filter name and @TeamA@ for the filter value.
--
-- 'keyNames', 'describeKeyPairs_keyNames' - The key pair names.
--
-- Default: Describes all your key pairs.
newDescribeKeyPairs ::
  DescribeKeyPairs
newDescribeKeyPairs =
  DescribeKeyPairs'
    { dryRun = Core.Nothing,
      keyPairIds = Core.Nothing,
      filters = Core.Nothing,
      keyNames = Core.Nothing
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeKeyPairs_dryRun :: Lens.Lens' DescribeKeyPairs (Core.Maybe Core.Bool)
describeKeyPairs_dryRun = Lens.lens (\DescribeKeyPairs' {dryRun} -> dryRun) (\s@DescribeKeyPairs' {} a -> s {dryRun = a} :: DescribeKeyPairs)

-- | The IDs of the key pairs.
describeKeyPairs_keyPairIds :: Lens.Lens' DescribeKeyPairs (Core.Maybe [Core.Text])
describeKeyPairs_keyPairIds = Lens.lens (\DescribeKeyPairs' {keyPairIds} -> keyPairIds) (\s@DescribeKeyPairs' {} a -> s {keyPairIds = a} :: DescribeKeyPairs) Core.. Lens.mapping Lens._Coerce

-- | The filters.
--
-- -   @key-pair-id@ - The ID of the key pair.
--
-- -   @fingerprint@ - The fingerprint of the key pair.
--
-- -   @key-name@ - The name of the key pair.
--
-- -   @tag-key@ - The key of a tag assigned to the resource. Use this
--     filter to find all resources assigned a tag with a specific key,
--     regardless of the tag value.
--
-- -   @tag@:\<key> - The key\/value combination of a tag assigned to the
--     resource. Use the tag key in the filter name and the tag value as
--     the filter value. For example, to find all resources that have a tag
--     with the key @Owner@ and the value @TeamA@, specify @tag:Owner@ for
--     the filter name and @TeamA@ for the filter value.
describeKeyPairs_filters :: Lens.Lens' DescribeKeyPairs (Core.Maybe [Filter])
describeKeyPairs_filters = Lens.lens (\DescribeKeyPairs' {filters} -> filters) (\s@DescribeKeyPairs' {} a -> s {filters = a} :: DescribeKeyPairs) Core.. Lens.mapping Lens._Coerce

-- | The key pair names.
--
-- Default: Describes all your key pairs.
describeKeyPairs_keyNames :: Lens.Lens' DescribeKeyPairs (Core.Maybe [Core.Text])
describeKeyPairs_keyNames = Lens.lens (\DescribeKeyPairs' {keyNames} -> keyNames) (\s@DescribeKeyPairs' {} a -> s {keyNames = a} :: DescribeKeyPairs) Core.. Lens.mapping Lens._Coerce

instance Core.AWSRequest DescribeKeyPairs where
  type
    AWSResponse DescribeKeyPairs =
      DescribeKeyPairsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeKeyPairsResponse'
            Core.<$> ( x Core..@? "keySet" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "item")
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeKeyPairs

instance Core.NFData DescribeKeyPairs

instance Core.ToHeaders DescribeKeyPairs where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeKeyPairs where
  toPath = Core.const "/"

instance Core.ToQuery DescribeKeyPairs where
  toQuery DescribeKeyPairs' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DescribeKeyPairs" :: Core.ByteString),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "DryRun" Core.=: dryRun,
        Core.toQuery
          (Core.toQueryList "KeyPairId" Core.<$> keyPairIds),
        Core.toQuery
          (Core.toQueryList "Filter" Core.<$> filters),
        Core.toQuery
          (Core.toQueryList "KeyName" Core.<$> keyNames)
      ]

-- | /See:/ 'newDescribeKeyPairsResponse' smart constructor.
data DescribeKeyPairsResponse = DescribeKeyPairsResponse'
  { -- | Information about the key pairs.
    keyPairs :: Core.Maybe [KeyPairInfo],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeKeyPairsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'keyPairs', 'describeKeyPairsResponse_keyPairs' - Information about the key pairs.
--
-- 'httpStatus', 'describeKeyPairsResponse_httpStatus' - The response's http status code.
newDescribeKeyPairsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeKeyPairsResponse
newDescribeKeyPairsResponse pHttpStatus_ =
  DescribeKeyPairsResponse'
    { keyPairs = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the key pairs.
describeKeyPairsResponse_keyPairs :: Lens.Lens' DescribeKeyPairsResponse (Core.Maybe [KeyPairInfo])
describeKeyPairsResponse_keyPairs = Lens.lens (\DescribeKeyPairsResponse' {keyPairs} -> keyPairs) (\s@DescribeKeyPairsResponse' {} a -> s {keyPairs = a} :: DescribeKeyPairsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeKeyPairsResponse_httpStatus :: Lens.Lens' DescribeKeyPairsResponse Core.Int
describeKeyPairsResponse_httpStatus = Lens.lens (\DescribeKeyPairsResponse' {httpStatus} -> httpStatus) (\s@DescribeKeyPairsResponse' {} a -> s {httpStatus = a} :: DescribeKeyPairsResponse)

instance Core.NFData DescribeKeyPairsResponse
