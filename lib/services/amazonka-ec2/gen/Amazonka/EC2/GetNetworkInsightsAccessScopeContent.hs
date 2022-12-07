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
-- Module      : Amazonka.EC2.GetNetworkInsightsAccessScopeContent
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the content for the specified Network Access Scope.
module Amazonka.EC2.GetNetworkInsightsAccessScopeContent
  ( -- * Creating a Request
    GetNetworkInsightsAccessScopeContent (..),
    newGetNetworkInsightsAccessScopeContent,

    -- * Request Lenses
    getNetworkInsightsAccessScopeContent_dryRun,
    getNetworkInsightsAccessScopeContent_networkInsightsAccessScopeId,

    -- * Destructuring the Response
    GetNetworkInsightsAccessScopeContentResponse (..),
    newGetNetworkInsightsAccessScopeContentResponse,

    -- * Response Lenses
    getNetworkInsightsAccessScopeContentResponse_networkInsightsAccessScopeContent,
    getNetworkInsightsAccessScopeContentResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetNetworkInsightsAccessScopeContent' smart constructor.
data GetNetworkInsightsAccessScopeContent = GetNetworkInsightsAccessScopeContent'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the Network Access Scope.
    networkInsightsAccessScopeId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetNetworkInsightsAccessScopeContent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'getNetworkInsightsAccessScopeContent_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'networkInsightsAccessScopeId', 'getNetworkInsightsAccessScopeContent_networkInsightsAccessScopeId' - The ID of the Network Access Scope.
newGetNetworkInsightsAccessScopeContent ::
  -- | 'networkInsightsAccessScopeId'
  Prelude.Text ->
  GetNetworkInsightsAccessScopeContent
newGetNetworkInsightsAccessScopeContent
  pNetworkInsightsAccessScopeId_ =
    GetNetworkInsightsAccessScopeContent'
      { dryRun =
          Prelude.Nothing,
        networkInsightsAccessScopeId =
          pNetworkInsightsAccessScopeId_
      }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
getNetworkInsightsAccessScopeContent_dryRun :: Lens.Lens' GetNetworkInsightsAccessScopeContent (Prelude.Maybe Prelude.Bool)
getNetworkInsightsAccessScopeContent_dryRun = Lens.lens (\GetNetworkInsightsAccessScopeContent' {dryRun} -> dryRun) (\s@GetNetworkInsightsAccessScopeContent' {} a -> s {dryRun = a} :: GetNetworkInsightsAccessScopeContent)

-- | The ID of the Network Access Scope.
getNetworkInsightsAccessScopeContent_networkInsightsAccessScopeId :: Lens.Lens' GetNetworkInsightsAccessScopeContent Prelude.Text
getNetworkInsightsAccessScopeContent_networkInsightsAccessScopeId = Lens.lens (\GetNetworkInsightsAccessScopeContent' {networkInsightsAccessScopeId} -> networkInsightsAccessScopeId) (\s@GetNetworkInsightsAccessScopeContent' {} a -> s {networkInsightsAccessScopeId = a} :: GetNetworkInsightsAccessScopeContent)

instance
  Core.AWSRequest
    GetNetworkInsightsAccessScopeContent
  where
  type
    AWSResponse GetNetworkInsightsAccessScopeContent =
      GetNetworkInsightsAccessScopeContentResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          GetNetworkInsightsAccessScopeContentResponse'
            Prelude.<$> (x Data..@? "networkInsightsAccessScopeContent")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetNetworkInsightsAccessScopeContent
  where
  hashWithSalt
    _salt
    GetNetworkInsightsAccessScopeContent' {..} =
      _salt `Prelude.hashWithSalt` dryRun
        `Prelude.hashWithSalt` networkInsightsAccessScopeId

instance
  Prelude.NFData
    GetNetworkInsightsAccessScopeContent
  where
  rnf GetNetworkInsightsAccessScopeContent' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf networkInsightsAccessScopeId

instance
  Data.ToHeaders
    GetNetworkInsightsAccessScopeContent
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    GetNetworkInsightsAccessScopeContent
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    GetNetworkInsightsAccessScopeContent
  where
  toQuery GetNetworkInsightsAccessScopeContent' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "GetNetworkInsightsAccessScopeContent" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        "NetworkInsightsAccessScopeId"
          Data.=: networkInsightsAccessScopeId
      ]

-- | /See:/ 'newGetNetworkInsightsAccessScopeContentResponse' smart constructor.
data GetNetworkInsightsAccessScopeContentResponse = GetNetworkInsightsAccessScopeContentResponse'
  { -- | The Network Access Scope content.
    networkInsightsAccessScopeContent :: Prelude.Maybe NetworkInsightsAccessScopeContent,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetNetworkInsightsAccessScopeContentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'networkInsightsAccessScopeContent', 'getNetworkInsightsAccessScopeContentResponse_networkInsightsAccessScopeContent' - The Network Access Scope content.
--
-- 'httpStatus', 'getNetworkInsightsAccessScopeContentResponse_httpStatus' - The response's http status code.
newGetNetworkInsightsAccessScopeContentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetNetworkInsightsAccessScopeContentResponse
newGetNetworkInsightsAccessScopeContentResponse
  pHttpStatus_ =
    GetNetworkInsightsAccessScopeContentResponse'
      { networkInsightsAccessScopeContent =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The Network Access Scope content.
getNetworkInsightsAccessScopeContentResponse_networkInsightsAccessScopeContent :: Lens.Lens' GetNetworkInsightsAccessScopeContentResponse (Prelude.Maybe NetworkInsightsAccessScopeContent)
getNetworkInsightsAccessScopeContentResponse_networkInsightsAccessScopeContent = Lens.lens (\GetNetworkInsightsAccessScopeContentResponse' {networkInsightsAccessScopeContent} -> networkInsightsAccessScopeContent) (\s@GetNetworkInsightsAccessScopeContentResponse' {} a -> s {networkInsightsAccessScopeContent = a} :: GetNetworkInsightsAccessScopeContentResponse)

-- | The response's http status code.
getNetworkInsightsAccessScopeContentResponse_httpStatus :: Lens.Lens' GetNetworkInsightsAccessScopeContentResponse Prelude.Int
getNetworkInsightsAccessScopeContentResponse_httpStatus = Lens.lens (\GetNetworkInsightsAccessScopeContentResponse' {httpStatus} -> httpStatus) (\s@GetNetworkInsightsAccessScopeContentResponse' {} a -> s {httpStatus = a} :: GetNetworkInsightsAccessScopeContentResponse)

instance
  Prelude.NFData
    GetNetworkInsightsAccessScopeContentResponse
  where
  rnf GetNetworkInsightsAccessScopeContentResponse' {..} =
    Prelude.rnf networkInsightsAccessScopeContent
      `Prelude.seq` Prelude.rnf httpStatus
