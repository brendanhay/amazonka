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
-- Module      : Amazonka.Lightsail.DeleteDistribution
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes your Amazon Lightsail content delivery network (CDN)
-- distribution.
module Amazonka.Lightsail.DeleteDistribution
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lightsail.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteDistribution' smart constructor.
data DeleteDistribution = DeleteDistribution'
  { -- | The name of the distribution to delete.
    --
    -- Use the @GetDistributions@ action to get a list of distribution names
    -- that you can specify.
    distributionName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
        Prelude.Nothing
    }

-- | The name of the distribution to delete.
--
-- Use the @GetDistributions@ action to get a list of distribution names
-- that you can specify.
deleteDistribution_distributionName :: Lens.Lens' DeleteDistribution (Prelude.Maybe Prelude.Text)
deleteDistribution_distributionName = Lens.lens (\DeleteDistribution' {distributionName} -> distributionName) (\s@DeleteDistribution' {} a -> s {distributionName = a} :: DeleteDistribution)

instance Core.AWSRequest DeleteDistribution where
  type
    AWSResponse DeleteDistribution =
      DeleteDistributionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteDistributionResponse'
            Prelude.<$> (x Data..?> "operation")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteDistribution where
  hashWithSalt _salt DeleteDistribution' {..} =
    _salt `Prelude.hashWithSalt` distributionName

instance Prelude.NFData DeleteDistribution where
  rnf DeleteDistribution' {..} =
    Prelude.rnf distributionName

instance Data.ToHeaders DeleteDistribution where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Lightsail_20161128.DeleteDistribution" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteDistribution where
  toJSON DeleteDistribution' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("distributionName" Data..=)
              Prelude.<$> distributionName
          ]
      )

instance Data.ToPath DeleteDistribution where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteDistribution where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteDistributionResponse' smart constructor.
data DeleteDistributionResponse = DeleteDistributionResponse'
  { -- | An object that describes the result of the action, such as the status of
    -- the request, the timestamp of the request, and the resources affected by
    -- the request.
    operation :: Prelude.Maybe Operation,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DeleteDistributionResponse
newDeleteDistributionResponse pHttpStatus_ =
  DeleteDistributionResponse'
    { operation =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An object that describes the result of the action, such as the status of
-- the request, the timestamp of the request, and the resources affected by
-- the request.
deleteDistributionResponse_operation :: Lens.Lens' DeleteDistributionResponse (Prelude.Maybe Operation)
deleteDistributionResponse_operation = Lens.lens (\DeleteDistributionResponse' {operation} -> operation) (\s@DeleteDistributionResponse' {} a -> s {operation = a} :: DeleteDistributionResponse)

-- | The response's http status code.
deleteDistributionResponse_httpStatus :: Lens.Lens' DeleteDistributionResponse Prelude.Int
deleteDistributionResponse_httpStatus = Lens.lens (\DeleteDistributionResponse' {httpStatus} -> httpStatus) (\s@DeleteDistributionResponse' {} a -> s {httpStatus = a} :: DeleteDistributionResponse)

instance Prelude.NFData DeleteDistributionResponse where
  rnf DeleteDistributionResponse' {..} =
    Prelude.rnf operation
      `Prelude.seq` Prelude.rnf httpStatus
