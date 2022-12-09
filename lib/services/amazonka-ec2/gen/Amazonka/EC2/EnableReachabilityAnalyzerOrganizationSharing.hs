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
-- Module      : Amazonka.EC2.EnableReachabilityAnalyzerOrganizationSharing
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Establishes a trust relationship between Reachability Analyzer and
-- Organizations. This operation must be performed by the management
-- account for the organization.
--
-- After you establish a trust relationship, a user in the management
-- account or a delegated administrator account can run a cross-account
-- analysis using resources from the member accounts.
module Amazonka.EC2.EnableReachabilityAnalyzerOrganizationSharing
  ( -- * Creating a Request
    EnableReachabilityAnalyzerOrganizationSharing (..),
    newEnableReachabilityAnalyzerOrganizationSharing,

    -- * Request Lenses
    enableReachabilityAnalyzerOrganizationSharing_dryRun,

    -- * Destructuring the Response
    EnableReachabilityAnalyzerOrganizationSharingResponse (..),
    newEnableReachabilityAnalyzerOrganizationSharingResponse,

    -- * Response Lenses
    enableReachabilityAnalyzerOrganizationSharingResponse_returnValue,
    enableReachabilityAnalyzerOrganizationSharingResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newEnableReachabilityAnalyzerOrganizationSharing' smart constructor.
data EnableReachabilityAnalyzerOrganizationSharing = EnableReachabilityAnalyzerOrganizationSharing'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EnableReachabilityAnalyzerOrganizationSharing' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'enableReachabilityAnalyzerOrganizationSharing_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
newEnableReachabilityAnalyzerOrganizationSharing ::
  EnableReachabilityAnalyzerOrganizationSharing
newEnableReachabilityAnalyzerOrganizationSharing =
  EnableReachabilityAnalyzerOrganizationSharing'
    { dryRun =
        Prelude.Nothing
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
enableReachabilityAnalyzerOrganizationSharing_dryRun :: Lens.Lens' EnableReachabilityAnalyzerOrganizationSharing (Prelude.Maybe Prelude.Bool)
enableReachabilityAnalyzerOrganizationSharing_dryRun = Lens.lens (\EnableReachabilityAnalyzerOrganizationSharing' {dryRun} -> dryRun) (\s@EnableReachabilityAnalyzerOrganizationSharing' {} a -> s {dryRun = a} :: EnableReachabilityAnalyzerOrganizationSharing)

instance
  Core.AWSRequest
    EnableReachabilityAnalyzerOrganizationSharing
  where
  type
    AWSResponse
      EnableReachabilityAnalyzerOrganizationSharing =
      EnableReachabilityAnalyzerOrganizationSharingResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          EnableReachabilityAnalyzerOrganizationSharingResponse'
            Prelude.<$> (x Data..@? "returnValue")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    EnableReachabilityAnalyzerOrganizationSharing
  where
  hashWithSalt
    _salt
    EnableReachabilityAnalyzerOrganizationSharing' {..} =
      _salt `Prelude.hashWithSalt` dryRun

instance
  Prelude.NFData
    EnableReachabilityAnalyzerOrganizationSharing
  where
  rnf
    EnableReachabilityAnalyzerOrganizationSharing' {..} =
      Prelude.rnf dryRun

instance
  Data.ToHeaders
    EnableReachabilityAnalyzerOrganizationSharing
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    EnableReachabilityAnalyzerOrganizationSharing
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    EnableReachabilityAnalyzerOrganizationSharing
  where
  toQuery
    EnableReachabilityAnalyzerOrganizationSharing' {..} =
      Prelude.mconcat
        [ "Action"
            Data.=: ( "EnableReachabilityAnalyzerOrganizationSharing" ::
                        Prelude.ByteString
                    ),
          "Version"
            Data.=: ("2016-11-15" :: Prelude.ByteString),
          "DryRun" Data.=: dryRun
        ]

-- | /See:/ 'newEnableReachabilityAnalyzerOrganizationSharingResponse' smart constructor.
data EnableReachabilityAnalyzerOrganizationSharingResponse = EnableReachabilityAnalyzerOrganizationSharingResponse'
  { -- | Returns @true@ if the request succeeds; otherwise, returns an error.
    returnValue :: Prelude.Maybe Prelude.Bool,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EnableReachabilityAnalyzerOrganizationSharingResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'returnValue', 'enableReachabilityAnalyzerOrganizationSharingResponse_returnValue' - Returns @true@ if the request succeeds; otherwise, returns an error.
--
-- 'httpStatus', 'enableReachabilityAnalyzerOrganizationSharingResponse_httpStatus' - The response's http status code.
newEnableReachabilityAnalyzerOrganizationSharingResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  EnableReachabilityAnalyzerOrganizationSharingResponse
newEnableReachabilityAnalyzerOrganizationSharingResponse
  pHttpStatus_ =
    EnableReachabilityAnalyzerOrganizationSharingResponse'
      { returnValue =
          Prelude.Nothing,
        httpStatus =
          pHttpStatus_
      }

-- | Returns @true@ if the request succeeds; otherwise, returns an error.
enableReachabilityAnalyzerOrganizationSharingResponse_returnValue :: Lens.Lens' EnableReachabilityAnalyzerOrganizationSharingResponse (Prelude.Maybe Prelude.Bool)
enableReachabilityAnalyzerOrganizationSharingResponse_returnValue = Lens.lens (\EnableReachabilityAnalyzerOrganizationSharingResponse' {returnValue} -> returnValue) (\s@EnableReachabilityAnalyzerOrganizationSharingResponse' {} a -> s {returnValue = a} :: EnableReachabilityAnalyzerOrganizationSharingResponse)

-- | The response's http status code.
enableReachabilityAnalyzerOrganizationSharingResponse_httpStatus :: Lens.Lens' EnableReachabilityAnalyzerOrganizationSharingResponse Prelude.Int
enableReachabilityAnalyzerOrganizationSharingResponse_httpStatus = Lens.lens (\EnableReachabilityAnalyzerOrganizationSharingResponse' {httpStatus} -> httpStatus) (\s@EnableReachabilityAnalyzerOrganizationSharingResponse' {} a -> s {httpStatus = a} :: EnableReachabilityAnalyzerOrganizationSharingResponse)

instance
  Prelude.NFData
    EnableReachabilityAnalyzerOrganizationSharingResponse
  where
  rnf
    EnableReachabilityAnalyzerOrganizationSharingResponse' {..} =
      Prelude.rnf returnValue
        `Prelude.seq` Prelude.rnf httpStatus
