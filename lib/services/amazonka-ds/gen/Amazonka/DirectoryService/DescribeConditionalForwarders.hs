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
-- Module      : Amazonka.DirectoryService.DescribeConditionalForwarders
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Obtains information about the conditional forwarders for this account.
--
-- If no input parameters are provided for RemoteDomainNames, this request
-- describes all conditional forwarders for the specified directory ID.
module Amazonka.DirectoryService.DescribeConditionalForwarders
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DirectoryService.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Describes a conditional forwarder.
--
-- /See:/ 'newDescribeConditionalForwarders' smart constructor.
data DescribeConditionalForwarders = DescribeConditionalForwarders'
  { -- | The fully qualified domain names (FQDN) of the remote domains for which
    -- to get the list of associated conditional forwarders. If this member is
    -- null, all conditional forwarders are returned.
    remoteDomainNames :: Prelude.Maybe [Prelude.Text],
    -- | The directory ID for which to get the list of associated conditional
    -- forwarders.
    directoryId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  DescribeConditionalForwarders
newDescribeConditionalForwarders pDirectoryId_ =
  DescribeConditionalForwarders'
    { remoteDomainNames =
        Prelude.Nothing,
      directoryId = pDirectoryId_
    }

-- | The fully qualified domain names (FQDN) of the remote domains for which
-- to get the list of associated conditional forwarders. If this member is
-- null, all conditional forwarders are returned.
describeConditionalForwarders_remoteDomainNames :: Lens.Lens' DescribeConditionalForwarders (Prelude.Maybe [Prelude.Text])
describeConditionalForwarders_remoteDomainNames = Lens.lens (\DescribeConditionalForwarders' {remoteDomainNames} -> remoteDomainNames) (\s@DescribeConditionalForwarders' {} a -> s {remoteDomainNames = a} :: DescribeConditionalForwarders) Prelude.. Lens.mapping Lens.coerced

-- | The directory ID for which to get the list of associated conditional
-- forwarders.
describeConditionalForwarders_directoryId :: Lens.Lens' DescribeConditionalForwarders Prelude.Text
describeConditionalForwarders_directoryId = Lens.lens (\DescribeConditionalForwarders' {directoryId} -> directoryId) (\s@DescribeConditionalForwarders' {} a -> s {directoryId = a} :: DescribeConditionalForwarders)

instance
  Core.AWSRequest
    DescribeConditionalForwarders
  where
  type
    AWSResponse DescribeConditionalForwarders =
      DescribeConditionalForwardersResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeConditionalForwardersResponse'
            Prelude.<$> ( x
                            Data..?> "ConditionalForwarders"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeConditionalForwarders
  where
  hashWithSalt _salt DescribeConditionalForwarders' {..} =
    _salt
      `Prelude.hashWithSalt` remoteDomainNames
      `Prelude.hashWithSalt` directoryId

instance Prelude.NFData DescribeConditionalForwarders where
  rnf DescribeConditionalForwarders' {..} =
    Prelude.rnf remoteDomainNames `Prelude.seq`
      Prelude.rnf directoryId

instance Data.ToHeaders DescribeConditionalForwarders where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "DirectoryService_20150416.DescribeConditionalForwarders" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeConditionalForwarders where
  toJSON DescribeConditionalForwarders' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("RemoteDomainNames" Data..=)
              Prelude.<$> remoteDomainNames,
            Prelude.Just ("DirectoryId" Data..= directoryId)
          ]
      )

instance Data.ToPath DescribeConditionalForwarders where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeConditionalForwarders where
  toQuery = Prelude.const Prelude.mempty

-- | The result of a DescribeConditionalForwarder request.
--
-- /See:/ 'newDescribeConditionalForwardersResponse' smart constructor.
data DescribeConditionalForwardersResponse = DescribeConditionalForwardersResponse'
  { -- | The list of conditional forwarders that have been created.
    conditionalForwarders :: Prelude.Maybe [ConditionalForwarder],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DescribeConditionalForwardersResponse
newDescribeConditionalForwardersResponse pHttpStatus_ =
  DescribeConditionalForwardersResponse'
    { conditionalForwarders =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The list of conditional forwarders that have been created.
describeConditionalForwardersResponse_conditionalForwarders :: Lens.Lens' DescribeConditionalForwardersResponse (Prelude.Maybe [ConditionalForwarder])
describeConditionalForwardersResponse_conditionalForwarders = Lens.lens (\DescribeConditionalForwardersResponse' {conditionalForwarders} -> conditionalForwarders) (\s@DescribeConditionalForwardersResponse' {} a -> s {conditionalForwarders = a} :: DescribeConditionalForwardersResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeConditionalForwardersResponse_httpStatus :: Lens.Lens' DescribeConditionalForwardersResponse Prelude.Int
describeConditionalForwardersResponse_httpStatus = Lens.lens (\DescribeConditionalForwardersResponse' {httpStatus} -> httpStatus) (\s@DescribeConditionalForwardersResponse' {} a -> s {httpStatus = a} :: DescribeConditionalForwardersResponse)

instance
  Prelude.NFData
    DescribeConditionalForwardersResponse
  where
  rnf DescribeConditionalForwardersResponse' {..} =
    Prelude.rnf conditionalForwarders `Prelude.seq`
      Prelude.rnf httpStatus
