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
-- Module      : Amazonka.SecurityHub.DeleteFindingAggregator
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a finding aggregator. When you delete the finding aggregator,
-- you stop finding aggregation.
--
-- When you stop finding aggregation, findings that were already aggregated
-- to the aggregation Region are still visible from the aggregation Region.
-- New findings and finding updates are not aggregated.
module Amazonka.SecurityHub.DeleteFindingAggregator
  ( -- * Creating a Request
    DeleteFindingAggregator (..),
    newDeleteFindingAggregator,

    -- * Request Lenses
    deleteFindingAggregator_findingAggregatorArn,

    -- * Destructuring the Response
    DeleteFindingAggregatorResponse (..),
    newDeleteFindingAggregatorResponse,

    -- * Response Lenses
    deleteFindingAggregatorResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SecurityHub.Types

-- | /See:/ 'newDeleteFindingAggregator' smart constructor.
data DeleteFindingAggregator = DeleteFindingAggregator'
  { -- | The ARN of the finding aggregator to delete. To obtain the ARN, use
    -- @ListFindingAggregators@.
    findingAggregatorArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteFindingAggregator' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'findingAggregatorArn', 'deleteFindingAggregator_findingAggregatorArn' - The ARN of the finding aggregator to delete. To obtain the ARN, use
-- @ListFindingAggregators@.
newDeleteFindingAggregator ::
  -- | 'findingAggregatorArn'
  Prelude.Text ->
  DeleteFindingAggregator
newDeleteFindingAggregator pFindingAggregatorArn_ =
  DeleteFindingAggregator'
    { findingAggregatorArn =
        pFindingAggregatorArn_
    }

-- | The ARN of the finding aggregator to delete. To obtain the ARN, use
-- @ListFindingAggregators@.
deleteFindingAggregator_findingAggregatorArn :: Lens.Lens' DeleteFindingAggregator Prelude.Text
deleteFindingAggregator_findingAggregatorArn = Lens.lens (\DeleteFindingAggregator' {findingAggregatorArn} -> findingAggregatorArn) (\s@DeleteFindingAggregator' {} a -> s {findingAggregatorArn = a} :: DeleteFindingAggregator)

instance Core.AWSRequest DeleteFindingAggregator where
  type
    AWSResponse DeleteFindingAggregator =
      DeleteFindingAggregatorResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteFindingAggregatorResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteFindingAggregator where
  hashWithSalt _salt DeleteFindingAggregator' {..} =
    _salt `Prelude.hashWithSalt` findingAggregatorArn

instance Prelude.NFData DeleteFindingAggregator where
  rnf DeleteFindingAggregator' {..} =
    Prelude.rnf findingAggregatorArn

instance Data.ToHeaders DeleteFindingAggregator where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteFindingAggregator where
  toPath DeleteFindingAggregator' {..} =
    Prelude.mconcat
      [ "/findingAggregator/delete/",
        Data.toBS findingAggregatorArn
      ]

instance Data.ToQuery DeleteFindingAggregator where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteFindingAggregatorResponse' smart constructor.
data DeleteFindingAggregatorResponse = DeleteFindingAggregatorResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteFindingAggregatorResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteFindingAggregatorResponse_httpStatus' - The response's http status code.
newDeleteFindingAggregatorResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteFindingAggregatorResponse
newDeleteFindingAggregatorResponse pHttpStatus_ =
  DeleteFindingAggregatorResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteFindingAggregatorResponse_httpStatus :: Lens.Lens' DeleteFindingAggregatorResponse Prelude.Int
deleteFindingAggregatorResponse_httpStatus = Lens.lens (\DeleteFindingAggregatorResponse' {httpStatus} -> httpStatus) (\s@DeleteFindingAggregatorResponse' {} a -> s {httpStatus = a} :: DeleteFindingAggregatorResponse)

instance
  Prelude.NFData
    DeleteFindingAggregatorResponse
  where
  rnf DeleteFindingAggregatorResponse' {..} =
    Prelude.rnf httpStatus
