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
-- Module      : Amazonka.DataExchange.DeleteRevision
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation deletes a revision.
module Amazonka.DataExchange.DeleteRevision
  ( -- * Creating a Request
    DeleteRevision (..),
    newDeleteRevision,

    -- * Request Lenses
    deleteRevision_dataSetId,
    deleteRevision_revisionId,

    -- * Destructuring the Response
    DeleteRevisionResponse (..),
    newDeleteRevisionResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DataExchange.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteRevision' smart constructor.
data DeleteRevision = DeleteRevision'
  { -- | The unique identifier for a data set.
    dataSetId :: Prelude.Text,
    -- | The unique identifier for a revision.
    revisionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteRevision' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataSetId', 'deleteRevision_dataSetId' - The unique identifier for a data set.
--
-- 'revisionId', 'deleteRevision_revisionId' - The unique identifier for a revision.
newDeleteRevision ::
  -- | 'dataSetId'
  Prelude.Text ->
  -- | 'revisionId'
  Prelude.Text ->
  DeleteRevision
newDeleteRevision pDataSetId_ pRevisionId_ =
  DeleteRevision'
    { dataSetId = pDataSetId_,
      revisionId = pRevisionId_
    }

-- | The unique identifier for a data set.
deleteRevision_dataSetId :: Lens.Lens' DeleteRevision Prelude.Text
deleteRevision_dataSetId = Lens.lens (\DeleteRevision' {dataSetId} -> dataSetId) (\s@DeleteRevision' {} a -> s {dataSetId = a} :: DeleteRevision)

-- | The unique identifier for a revision.
deleteRevision_revisionId :: Lens.Lens' DeleteRevision Prelude.Text
deleteRevision_revisionId = Lens.lens (\DeleteRevision' {revisionId} -> revisionId) (\s@DeleteRevision' {} a -> s {revisionId = a} :: DeleteRevision)

instance Core.AWSRequest DeleteRevision where
  type
    AWSResponse DeleteRevision =
      DeleteRevisionResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull DeleteRevisionResponse'

instance Prelude.Hashable DeleteRevision where
  hashWithSalt _salt DeleteRevision' {..} =
    _salt
      `Prelude.hashWithSalt` dataSetId
      `Prelude.hashWithSalt` revisionId

instance Prelude.NFData DeleteRevision where
  rnf DeleteRevision' {..} =
    Prelude.rnf dataSetId `Prelude.seq`
      Prelude.rnf revisionId

instance Data.ToHeaders DeleteRevision where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteRevision where
  toPath DeleteRevision' {..} =
    Prelude.mconcat
      [ "/v1/data-sets/",
        Data.toBS dataSetId,
        "/revisions/",
        Data.toBS revisionId
      ]

instance Data.ToQuery DeleteRevision where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteRevisionResponse' smart constructor.
data DeleteRevisionResponse = DeleteRevisionResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteRevisionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteRevisionResponse ::
  DeleteRevisionResponse
newDeleteRevisionResponse = DeleteRevisionResponse'

instance Prelude.NFData DeleteRevisionResponse where
  rnf _ = ()
