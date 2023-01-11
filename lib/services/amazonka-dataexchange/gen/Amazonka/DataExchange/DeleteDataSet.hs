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
-- Module      : Amazonka.DataExchange.DeleteDataSet
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation deletes a data set.
module Amazonka.DataExchange.DeleteDataSet
  ( -- * Creating a Request
    DeleteDataSet (..),
    newDeleteDataSet,

    -- * Request Lenses
    deleteDataSet_dataSetId,

    -- * Destructuring the Response
    DeleteDataSetResponse (..),
    newDeleteDataSetResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DataExchange.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteDataSet' smart constructor.
data DeleteDataSet = DeleteDataSet'
  { -- | The unique identifier for a data set.
    dataSetId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteDataSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataSetId', 'deleteDataSet_dataSetId' - The unique identifier for a data set.
newDeleteDataSet ::
  -- | 'dataSetId'
  Prelude.Text ->
  DeleteDataSet
newDeleteDataSet pDataSetId_ =
  DeleteDataSet' {dataSetId = pDataSetId_}

-- | The unique identifier for a data set.
deleteDataSet_dataSetId :: Lens.Lens' DeleteDataSet Prelude.Text
deleteDataSet_dataSetId = Lens.lens (\DeleteDataSet' {dataSetId} -> dataSetId) (\s@DeleteDataSet' {} a -> s {dataSetId = a} :: DeleteDataSet)

instance Core.AWSRequest DeleteDataSet where
  type
    AWSResponse DeleteDataSet =
      DeleteDataSetResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull DeleteDataSetResponse'

instance Prelude.Hashable DeleteDataSet where
  hashWithSalt _salt DeleteDataSet' {..} =
    _salt `Prelude.hashWithSalt` dataSetId

instance Prelude.NFData DeleteDataSet where
  rnf DeleteDataSet' {..} = Prelude.rnf dataSetId

instance Data.ToHeaders DeleteDataSet where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteDataSet where
  toPath DeleteDataSet' {..} =
    Prelude.mconcat
      ["/v1/data-sets/", Data.toBS dataSetId]

instance Data.ToQuery DeleteDataSet where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteDataSetResponse' smart constructor.
data DeleteDataSetResponse = DeleteDataSetResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteDataSetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteDataSetResponse ::
  DeleteDataSetResponse
newDeleteDataSetResponse = DeleteDataSetResponse'

instance Prelude.NFData DeleteDataSetResponse where
  rnf _ = ()
