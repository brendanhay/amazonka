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
-- Module      : Network.AWS.DirectoryService.DescribeConditionalForwarders
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Obtains information about the conditional forwarders for this account.
--
-- If no input parameters are provided for RemoteDomainNames, this request
-- describes all conditional forwarders for the specified directory ID.
module Network.AWS.DirectoryService.DescribeConditionalForwarders
  ( -- * Creating a Request
    DescribeConditionalForwarders (..),
    newDescribeConditionalForwarders,

    -- * Request Lenses
    describeConditionalForwarders_remoteDomainNames,
    describeConditionalForwarders_directoryId,

    -- * Destructuring the Response
    DescribeConditionalForwardersResponse (..),
    newDescribeConditionalForwardersResponse,

    -- * Response Lenses
    describeConditionalForwardersResponse_conditionalForwarders,
    describeConditionalForwardersResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DirectoryService.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Describes a conditional forwarder.
--
-- /See:/ 'newDescribeConditionalForwarders' smart constructor.
data DescribeConditionalForwarders = DescribeConditionalForwarders'
  { -- | The fully qualified domain names (FQDN) of the remote domains for which
    -- to get the list of associated conditional forwarders. If this member is
    -- null, all conditional forwarders are returned.
    remoteDomainNames :: Core.Maybe [Core.Text],
    -- | The directory ID for which to get the list of associated conditional
    -- forwarders.
    directoryId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeConditionalForwarders' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'remoteDomainNames', 'describeConditionalForwarders_remoteDomainNames' - The fully qualified domain names (FQDN) of the remote domains for which
-- to get the list of associated conditional forwarders. If this member is
-- null, all conditional forwarders are returned.
--
-- 'directoryId', 'describeConditionalForwarders_directoryId' - The directory ID for which to get the list of associated conditional
-- forwarders.
newDescribeConditionalForwarders ::
  -- | 'directoryId'
  Core.Text ->
  DescribeConditionalForwarders
newDescribeConditionalForwarders pDirectoryId_ =
  DescribeConditionalForwarders'
    { remoteDomainNames =
        Core.Nothing,
      directoryId = pDirectoryId_
    }

-- | The fully qualified domain names (FQDN) of the remote domains for which
-- to get the list of associated conditional forwarders. If this member is
-- null, all conditional forwarders are returned.
describeConditionalForwarders_remoteDomainNames :: Lens.Lens' DescribeConditionalForwarders (Core.Maybe [Core.Text])
describeConditionalForwarders_remoteDomainNames = Lens.lens (\DescribeConditionalForwarders' {remoteDomainNames} -> remoteDomainNames) (\s@DescribeConditionalForwarders' {} a -> s {remoteDomainNames = a} :: DescribeConditionalForwarders) Core.. Lens.mapping Lens._Coerce

-- | The directory ID for which to get the list of associated conditional
-- forwarders.
describeConditionalForwarders_directoryId :: Lens.Lens' DescribeConditionalForwarders Core.Text
describeConditionalForwarders_directoryId = Lens.lens (\DescribeConditionalForwarders' {directoryId} -> directoryId) (\s@DescribeConditionalForwarders' {} a -> s {directoryId = a} :: DescribeConditionalForwarders)

instance
  Core.AWSRequest
    DescribeConditionalForwarders
  where
  type
    AWSResponse DescribeConditionalForwarders =
      DescribeConditionalForwardersResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeConditionalForwardersResponse'
            Core.<$> ( x Core..?> "ConditionalForwarders"
                         Core..!@ Core.mempty
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeConditionalForwarders

instance Core.NFData DescribeConditionalForwarders

instance Core.ToHeaders DescribeConditionalForwarders where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DirectoryService_20150416.DescribeConditionalForwarders" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeConditionalForwarders where
  toJSON DescribeConditionalForwarders' {..} =
    Core.object
      ( Core.catMaybes
          [ ("RemoteDomainNames" Core..=)
              Core.<$> remoteDomainNames,
            Core.Just ("DirectoryId" Core..= directoryId)
          ]
      )

instance Core.ToPath DescribeConditionalForwarders where
  toPath = Core.const "/"

instance Core.ToQuery DescribeConditionalForwarders where
  toQuery = Core.const Core.mempty

-- | The result of a DescribeConditionalForwarder request.
--
-- /See:/ 'newDescribeConditionalForwardersResponse' smart constructor.
data DescribeConditionalForwardersResponse = DescribeConditionalForwardersResponse'
  { -- | The list of conditional forwarders that have been created.
    conditionalForwarders :: Core.Maybe [ConditionalForwarder],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeConditionalForwardersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'conditionalForwarders', 'describeConditionalForwardersResponse_conditionalForwarders' - The list of conditional forwarders that have been created.
--
-- 'httpStatus', 'describeConditionalForwardersResponse_httpStatus' - The response's http status code.
newDescribeConditionalForwardersResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeConditionalForwardersResponse
newDescribeConditionalForwardersResponse pHttpStatus_ =
  DescribeConditionalForwardersResponse'
    { conditionalForwarders =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The list of conditional forwarders that have been created.
describeConditionalForwardersResponse_conditionalForwarders :: Lens.Lens' DescribeConditionalForwardersResponse (Core.Maybe [ConditionalForwarder])
describeConditionalForwardersResponse_conditionalForwarders = Lens.lens (\DescribeConditionalForwardersResponse' {conditionalForwarders} -> conditionalForwarders) (\s@DescribeConditionalForwardersResponse' {} a -> s {conditionalForwarders = a} :: DescribeConditionalForwardersResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeConditionalForwardersResponse_httpStatus :: Lens.Lens' DescribeConditionalForwardersResponse Core.Int
describeConditionalForwardersResponse_httpStatus = Lens.lens (\DescribeConditionalForwardersResponse' {httpStatus} -> httpStatus) (\s@DescribeConditionalForwardersResponse' {} a -> s {httpStatus = a} :: DescribeConditionalForwardersResponse)

instance
  Core.NFData
    DescribeConditionalForwardersResponse
