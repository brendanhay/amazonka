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
-- Module      : Amazonka.GuardDuty.DeleteIPSet
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the IPSet specified by the @ipSetId@. IPSets are called trusted
-- IP lists in the console user interface.
module Amazonka.GuardDuty.DeleteIPSet
  ( -- * Creating a Request
    DeleteIPSet (..),
    newDeleteIPSet,

    -- * Request Lenses
    deleteIPSet_detectorId,
    deleteIPSet_ipSetId,

    -- * Destructuring the Response
    DeleteIPSetResponse (..),
    newDeleteIPSetResponse,

    -- * Response Lenses
    deleteIPSetResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GuardDuty.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteIPSet' smart constructor.
data DeleteIPSet = DeleteIPSet'
  { -- | The unique ID of the detector associated with the IPSet.
    detectorId :: Prelude.Text,
    -- | The unique ID of the IPSet to delete.
    ipSetId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteIPSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'detectorId', 'deleteIPSet_detectorId' - The unique ID of the detector associated with the IPSet.
--
-- 'ipSetId', 'deleteIPSet_ipSetId' - The unique ID of the IPSet to delete.
newDeleteIPSet ::
  -- | 'detectorId'
  Prelude.Text ->
  -- | 'ipSetId'
  Prelude.Text ->
  DeleteIPSet
newDeleteIPSet pDetectorId_ pIpSetId_ =
  DeleteIPSet'
    { detectorId = pDetectorId_,
      ipSetId = pIpSetId_
    }

-- | The unique ID of the detector associated with the IPSet.
deleteIPSet_detectorId :: Lens.Lens' DeleteIPSet Prelude.Text
deleteIPSet_detectorId = Lens.lens (\DeleteIPSet' {detectorId} -> detectorId) (\s@DeleteIPSet' {} a -> s {detectorId = a} :: DeleteIPSet)

-- | The unique ID of the IPSet to delete.
deleteIPSet_ipSetId :: Lens.Lens' DeleteIPSet Prelude.Text
deleteIPSet_ipSetId = Lens.lens (\DeleteIPSet' {ipSetId} -> ipSetId) (\s@DeleteIPSet' {} a -> s {ipSetId = a} :: DeleteIPSet)

instance Core.AWSRequest DeleteIPSet where
  type AWSResponse DeleteIPSet = DeleteIPSetResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteIPSetResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteIPSet where
  hashWithSalt _salt DeleteIPSet' {..} =
    _salt `Prelude.hashWithSalt` detectorId
      `Prelude.hashWithSalt` ipSetId

instance Prelude.NFData DeleteIPSet where
  rnf DeleteIPSet' {..} =
    Prelude.rnf detectorId
      `Prelude.seq` Prelude.rnf ipSetId

instance Data.ToHeaders DeleteIPSet where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteIPSet where
  toPath DeleteIPSet' {..} =
    Prelude.mconcat
      [ "/detector/",
        Data.toBS detectorId,
        "/ipset/",
        Data.toBS ipSetId
      ]

instance Data.ToQuery DeleteIPSet where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteIPSetResponse' smart constructor.
data DeleteIPSetResponse = DeleteIPSetResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteIPSetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteIPSetResponse_httpStatus' - The response's http status code.
newDeleteIPSetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteIPSetResponse
newDeleteIPSetResponse pHttpStatus_ =
  DeleteIPSetResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteIPSetResponse_httpStatus :: Lens.Lens' DeleteIPSetResponse Prelude.Int
deleteIPSetResponse_httpStatus = Lens.lens (\DeleteIPSetResponse' {httpStatus} -> httpStatus) (\s@DeleteIPSetResponse' {} a -> s {httpStatus = a} :: DeleteIPSetResponse)

instance Prelude.NFData DeleteIPSetResponse where
  rnf DeleteIPSetResponse' {..} = Prelude.rnf httpStatus
