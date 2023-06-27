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
-- Module      : Amazonka.LexV2Models.DeleteTestSet
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The action to delete the selected test set.
module Amazonka.LexV2Models.DeleteTestSet
  ( -- * Creating a Request
    DeleteTestSet (..),
    newDeleteTestSet,

    -- * Request Lenses
    deleteTestSet_testSetId,

    -- * Destructuring the Response
    DeleteTestSetResponse (..),
    newDeleteTestSetResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteTestSet' smart constructor.
data DeleteTestSet = DeleteTestSet'
  { -- | The test set Id of the test set to be deleted.
    testSetId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteTestSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'testSetId', 'deleteTestSet_testSetId' - The test set Id of the test set to be deleted.
newDeleteTestSet ::
  -- | 'testSetId'
  Prelude.Text ->
  DeleteTestSet
newDeleteTestSet pTestSetId_ =
  DeleteTestSet' {testSetId = pTestSetId_}

-- | The test set Id of the test set to be deleted.
deleteTestSet_testSetId :: Lens.Lens' DeleteTestSet Prelude.Text
deleteTestSet_testSetId = Lens.lens (\DeleteTestSet' {testSetId} -> testSetId) (\s@DeleteTestSet' {} a -> s {testSetId = a} :: DeleteTestSet)

instance Core.AWSRequest DeleteTestSet where
  type
    AWSResponse DeleteTestSet =
      DeleteTestSetResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull DeleteTestSetResponse'

instance Prelude.Hashable DeleteTestSet where
  hashWithSalt _salt DeleteTestSet' {..} =
    _salt `Prelude.hashWithSalt` testSetId

instance Prelude.NFData DeleteTestSet where
  rnf DeleteTestSet' {..} = Prelude.rnf testSetId

instance Data.ToHeaders DeleteTestSet where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteTestSet where
  toPath DeleteTestSet' {..} =
    Prelude.mconcat ["/testsets/", Data.toBS testSetId]

instance Data.ToQuery DeleteTestSet where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteTestSetResponse' smart constructor.
data DeleteTestSetResponse = DeleteTestSetResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteTestSetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteTestSetResponse ::
  DeleteTestSetResponse
newDeleteTestSetResponse = DeleteTestSetResponse'

instance Prelude.NFData DeleteTestSetResponse where
  rnf _ = ()
