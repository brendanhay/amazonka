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
-- Module      : Amazonka.EC2.DeleteNetworkInsightsAccessScope
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified Network Access Scope.
module Amazonka.EC2.DeleteNetworkInsightsAccessScope
  ( -- * Creating a Request
    DeleteNetworkInsightsAccessScope (..),
    newDeleteNetworkInsightsAccessScope,

    -- * Request Lenses
    deleteNetworkInsightsAccessScope_dryRun,
    deleteNetworkInsightsAccessScope_networkInsightsAccessScopeId,

    -- * Destructuring the Response
    DeleteNetworkInsightsAccessScopeResponse (..),
    newDeleteNetworkInsightsAccessScopeResponse,

    -- * Response Lenses
    deleteNetworkInsightsAccessScopeResponse_networkInsightsAccessScopeId,
    deleteNetworkInsightsAccessScopeResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteNetworkInsightsAccessScope' smart constructor.
data DeleteNetworkInsightsAccessScope = DeleteNetworkInsightsAccessScope'
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
-- Create a value of 'DeleteNetworkInsightsAccessScope' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'deleteNetworkInsightsAccessScope_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'networkInsightsAccessScopeId', 'deleteNetworkInsightsAccessScope_networkInsightsAccessScopeId' - The ID of the Network Access Scope.
newDeleteNetworkInsightsAccessScope ::
  -- | 'networkInsightsAccessScopeId'
  Prelude.Text ->
  DeleteNetworkInsightsAccessScope
newDeleteNetworkInsightsAccessScope
  pNetworkInsightsAccessScopeId_ =
    DeleteNetworkInsightsAccessScope'
      { dryRun =
          Prelude.Nothing,
        networkInsightsAccessScopeId =
          pNetworkInsightsAccessScopeId_
      }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
deleteNetworkInsightsAccessScope_dryRun :: Lens.Lens' DeleteNetworkInsightsAccessScope (Prelude.Maybe Prelude.Bool)
deleteNetworkInsightsAccessScope_dryRun = Lens.lens (\DeleteNetworkInsightsAccessScope' {dryRun} -> dryRun) (\s@DeleteNetworkInsightsAccessScope' {} a -> s {dryRun = a} :: DeleteNetworkInsightsAccessScope)

-- | The ID of the Network Access Scope.
deleteNetworkInsightsAccessScope_networkInsightsAccessScopeId :: Lens.Lens' DeleteNetworkInsightsAccessScope Prelude.Text
deleteNetworkInsightsAccessScope_networkInsightsAccessScopeId = Lens.lens (\DeleteNetworkInsightsAccessScope' {networkInsightsAccessScopeId} -> networkInsightsAccessScopeId) (\s@DeleteNetworkInsightsAccessScope' {} a -> s {networkInsightsAccessScopeId = a} :: DeleteNetworkInsightsAccessScope)

instance
  Core.AWSRequest
    DeleteNetworkInsightsAccessScope
  where
  type
    AWSResponse DeleteNetworkInsightsAccessScope =
      DeleteNetworkInsightsAccessScopeResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DeleteNetworkInsightsAccessScopeResponse'
            Prelude.<$> (x Data..@? "networkInsightsAccessScopeId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DeleteNetworkInsightsAccessScope
  where
  hashWithSalt
    _salt
    DeleteNetworkInsightsAccessScope' {..} =
      _salt
        `Prelude.hashWithSalt` dryRun
        `Prelude.hashWithSalt` networkInsightsAccessScopeId

instance
  Prelude.NFData
    DeleteNetworkInsightsAccessScope
  where
  rnf DeleteNetworkInsightsAccessScope' {..} =
    Prelude.rnf dryRun `Prelude.seq`
      Prelude.rnf networkInsightsAccessScopeId

instance
  Data.ToHeaders
    DeleteNetworkInsightsAccessScope
  where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeleteNetworkInsightsAccessScope where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DeleteNetworkInsightsAccessScope
  where
  toQuery DeleteNetworkInsightsAccessScope' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "DeleteNetworkInsightsAccessScope" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        "NetworkInsightsAccessScopeId"
          Data.=: networkInsightsAccessScopeId
      ]

-- | /See:/ 'newDeleteNetworkInsightsAccessScopeResponse' smart constructor.
data DeleteNetworkInsightsAccessScopeResponse = DeleteNetworkInsightsAccessScopeResponse'
  { -- | The ID of the Network Access Scope.
    networkInsightsAccessScopeId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteNetworkInsightsAccessScopeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'networkInsightsAccessScopeId', 'deleteNetworkInsightsAccessScopeResponse_networkInsightsAccessScopeId' - The ID of the Network Access Scope.
--
-- 'httpStatus', 'deleteNetworkInsightsAccessScopeResponse_httpStatus' - The response's http status code.
newDeleteNetworkInsightsAccessScopeResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteNetworkInsightsAccessScopeResponse
newDeleteNetworkInsightsAccessScopeResponse
  pHttpStatus_ =
    DeleteNetworkInsightsAccessScopeResponse'
      { networkInsightsAccessScopeId =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The ID of the Network Access Scope.
deleteNetworkInsightsAccessScopeResponse_networkInsightsAccessScopeId :: Lens.Lens' DeleteNetworkInsightsAccessScopeResponse (Prelude.Maybe Prelude.Text)
deleteNetworkInsightsAccessScopeResponse_networkInsightsAccessScopeId = Lens.lens (\DeleteNetworkInsightsAccessScopeResponse' {networkInsightsAccessScopeId} -> networkInsightsAccessScopeId) (\s@DeleteNetworkInsightsAccessScopeResponse' {} a -> s {networkInsightsAccessScopeId = a} :: DeleteNetworkInsightsAccessScopeResponse)

-- | The response's http status code.
deleteNetworkInsightsAccessScopeResponse_httpStatus :: Lens.Lens' DeleteNetworkInsightsAccessScopeResponse Prelude.Int
deleteNetworkInsightsAccessScopeResponse_httpStatus = Lens.lens (\DeleteNetworkInsightsAccessScopeResponse' {httpStatus} -> httpStatus) (\s@DeleteNetworkInsightsAccessScopeResponse' {} a -> s {httpStatus = a} :: DeleteNetworkInsightsAccessScopeResponse)

instance
  Prelude.NFData
    DeleteNetworkInsightsAccessScopeResponse
  where
  rnf DeleteNetworkInsightsAccessScopeResponse' {..} =
    Prelude.rnf networkInsightsAccessScopeId `Prelude.seq`
      Prelude.rnf httpStatus
