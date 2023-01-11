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
-- Module      : Amazonka.EC2.DeleteNetworkInsightsPath
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified path.
module Amazonka.EC2.DeleteNetworkInsightsPath
  ( -- * Creating a Request
    DeleteNetworkInsightsPath (..),
    newDeleteNetworkInsightsPath,

    -- * Request Lenses
    deleteNetworkInsightsPath_dryRun,
    deleteNetworkInsightsPath_networkInsightsPathId,

    -- * Destructuring the Response
    DeleteNetworkInsightsPathResponse (..),
    newDeleteNetworkInsightsPathResponse,

    -- * Response Lenses
    deleteNetworkInsightsPathResponse_networkInsightsPathId,
    deleteNetworkInsightsPathResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteNetworkInsightsPath' smart constructor.
data DeleteNetworkInsightsPath = DeleteNetworkInsightsPath'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the path.
    networkInsightsPathId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteNetworkInsightsPath' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'deleteNetworkInsightsPath_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'networkInsightsPathId', 'deleteNetworkInsightsPath_networkInsightsPathId' - The ID of the path.
newDeleteNetworkInsightsPath ::
  -- | 'networkInsightsPathId'
  Prelude.Text ->
  DeleteNetworkInsightsPath
newDeleteNetworkInsightsPath pNetworkInsightsPathId_ =
  DeleteNetworkInsightsPath'
    { dryRun =
        Prelude.Nothing,
      networkInsightsPathId = pNetworkInsightsPathId_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
deleteNetworkInsightsPath_dryRun :: Lens.Lens' DeleteNetworkInsightsPath (Prelude.Maybe Prelude.Bool)
deleteNetworkInsightsPath_dryRun = Lens.lens (\DeleteNetworkInsightsPath' {dryRun} -> dryRun) (\s@DeleteNetworkInsightsPath' {} a -> s {dryRun = a} :: DeleteNetworkInsightsPath)

-- | The ID of the path.
deleteNetworkInsightsPath_networkInsightsPathId :: Lens.Lens' DeleteNetworkInsightsPath Prelude.Text
deleteNetworkInsightsPath_networkInsightsPathId = Lens.lens (\DeleteNetworkInsightsPath' {networkInsightsPathId} -> networkInsightsPathId) (\s@DeleteNetworkInsightsPath' {} a -> s {networkInsightsPathId = a} :: DeleteNetworkInsightsPath)

instance Core.AWSRequest DeleteNetworkInsightsPath where
  type
    AWSResponse DeleteNetworkInsightsPath =
      DeleteNetworkInsightsPathResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DeleteNetworkInsightsPathResponse'
            Prelude.<$> (x Data..@? "networkInsightsPathId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteNetworkInsightsPath where
  hashWithSalt _salt DeleteNetworkInsightsPath' {..} =
    _salt `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` networkInsightsPathId

instance Prelude.NFData DeleteNetworkInsightsPath where
  rnf DeleteNetworkInsightsPath' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf networkInsightsPathId

instance Data.ToHeaders DeleteNetworkInsightsPath where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeleteNetworkInsightsPath where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteNetworkInsightsPath where
  toQuery DeleteNetworkInsightsPath' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DeleteNetworkInsightsPath" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        "NetworkInsightsPathId"
          Data.=: networkInsightsPathId
      ]

-- | /See:/ 'newDeleteNetworkInsightsPathResponse' smart constructor.
data DeleteNetworkInsightsPathResponse = DeleteNetworkInsightsPathResponse'
  { -- | The ID of the path.
    networkInsightsPathId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteNetworkInsightsPathResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'networkInsightsPathId', 'deleteNetworkInsightsPathResponse_networkInsightsPathId' - The ID of the path.
--
-- 'httpStatus', 'deleteNetworkInsightsPathResponse_httpStatus' - The response's http status code.
newDeleteNetworkInsightsPathResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteNetworkInsightsPathResponse
newDeleteNetworkInsightsPathResponse pHttpStatus_ =
  DeleteNetworkInsightsPathResponse'
    { networkInsightsPathId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID of the path.
deleteNetworkInsightsPathResponse_networkInsightsPathId :: Lens.Lens' DeleteNetworkInsightsPathResponse (Prelude.Maybe Prelude.Text)
deleteNetworkInsightsPathResponse_networkInsightsPathId = Lens.lens (\DeleteNetworkInsightsPathResponse' {networkInsightsPathId} -> networkInsightsPathId) (\s@DeleteNetworkInsightsPathResponse' {} a -> s {networkInsightsPathId = a} :: DeleteNetworkInsightsPathResponse)

-- | The response's http status code.
deleteNetworkInsightsPathResponse_httpStatus :: Lens.Lens' DeleteNetworkInsightsPathResponse Prelude.Int
deleteNetworkInsightsPathResponse_httpStatus = Lens.lens (\DeleteNetworkInsightsPathResponse' {httpStatus} -> httpStatus) (\s@DeleteNetworkInsightsPathResponse' {} a -> s {httpStatus = a} :: DeleteNetworkInsightsPathResponse)

instance
  Prelude.NFData
    DeleteNetworkInsightsPathResponse
  where
  rnf DeleteNetworkInsightsPathResponse' {..} =
    Prelude.rnf networkInsightsPathId
      `Prelude.seq` Prelude.rnf httpStatus
