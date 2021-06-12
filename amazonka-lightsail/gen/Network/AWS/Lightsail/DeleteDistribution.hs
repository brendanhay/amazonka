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
-- Module      : Network.AWS.Lightsail.DeleteDistribution
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes your Amazon Lightsail content delivery network (CDN)
-- distribution.
module Network.AWS.Lightsail.DeleteDistribution
  ( -- * Creating a Request
    DeleteDistribution (..),
    newDeleteDistribution,

    -- * Request Lenses
    deleteDistribution_distributionName,

    -- * Destructuring the Response
    DeleteDistributionResponse (..),
    newDeleteDistributionResponse,

    -- * Response Lenses
    deleteDistributionResponse_operation,
    deleteDistributionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteDistribution' smart constructor.
data DeleteDistribution = DeleteDistribution'
  { -- | The name of the distribution to delete.
    --
    -- Use the @GetDistributions@ action to get a list of distribution names
    -- that you can specify.
    distributionName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteDistribution' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'distributionName', 'deleteDistribution_distributionName' - The name of the distribution to delete.
--
-- Use the @GetDistributions@ action to get a list of distribution names
-- that you can specify.
newDeleteDistribution ::
  DeleteDistribution
newDeleteDistribution =
  DeleteDistribution'
    { distributionName =
        Core.Nothing
    }

-- | The name of the distribution to delete.
--
-- Use the @GetDistributions@ action to get a list of distribution names
-- that you can specify.
deleteDistribution_distributionName :: Lens.Lens' DeleteDistribution (Core.Maybe Core.Text)
deleteDistribution_distributionName = Lens.lens (\DeleteDistribution' {distributionName} -> distributionName) (\s@DeleteDistribution' {} a -> s {distributionName = a} :: DeleteDistribution)

instance Core.AWSRequest DeleteDistribution where
  type
    AWSResponse DeleteDistribution =
      DeleteDistributionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteDistributionResponse'
            Core.<$> (x Core..?> "operation")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteDistribution

instance Core.NFData DeleteDistribution

instance Core.ToHeaders DeleteDistribution where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Lightsail_20161128.DeleteDistribution" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteDistribution where
  toJSON DeleteDistribution' {..} =
    Core.object
      ( Core.catMaybes
          [ ("distributionName" Core..=)
              Core.<$> distributionName
          ]
      )

instance Core.ToPath DeleteDistribution where
  toPath = Core.const "/"

instance Core.ToQuery DeleteDistribution where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteDistributionResponse' smart constructor.
data DeleteDistributionResponse = DeleteDistributionResponse'
  { -- | An object that describes the result of the action, such as the status of
    -- the request, the timestamp of the request, and the resources affected by
    -- the request.
    operation :: Core.Maybe Operation,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteDistributionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'operation', 'deleteDistributionResponse_operation' - An object that describes the result of the action, such as the status of
-- the request, the timestamp of the request, and the resources affected by
-- the request.
--
-- 'httpStatus', 'deleteDistributionResponse_httpStatus' - The response's http status code.
newDeleteDistributionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteDistributionResponse
newDeleteDistributionResponse pHttpStatus_ =
  DeleteDistributionResponse'
    { operation =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An object that describes the result of the action, such as the status of
-- the request, the timestamp of the request, and the resources affected by
-- the request.
deleteDistributionResponse_operation :: Lens.Lens' DeleteDistributionResponse (Core.Maybe Operation)
deleteDistributionResponse_operation = Lens.lens (\DeleteDistributionResponse' {operation} -> operation) (\s@DeleteDistributionResponse' {} a -> s {operation = a} :: DeleteDistributionResponse)

-- | The response's http status code.
deleteDistributionResponse_httpStatus :: Lens.Lens' DeleteDistributionResponse Core.Int
deleteDistributionResponse_httpStatus = Lens.lens (\DeleteDistributionResponse' {httpStatus} -> httpStatus) (\s@DeleteDistributionResponse' {} a -> s {httpStatus = a} :: DeleteDistributionResponse)

instance Core.NFData DeleteDistributionResponse
