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
-- Module      : Amazonka.Location.DeleteTracker
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a tracker resource from your AWS account.
--
-- This operation deletes the resource permanently. If the tracker resource
-- is in use, you may encounter an error. Make sure that the target
-- resource isn\'t a dependency for your applications.
module Amazonka.Location.DeleteTracker
  ( -- * Creating a Request
    DeleteTracker (..),
    newDeleteTracker,

    -- * Request Lenses
    deleteTracker_trackerName,

    -- * Destructuring the Response
    DeleteTrackerResponse (..),
    newDeleteTrackerResponse,

    -- * Response Lenses
    deleteTrackerResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Location.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteTracker' smart constructor.
data DeleteTracker = DeleteTracker'
  { -- | The name of the tracker resource to be deleted.
    trackerName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteTracker' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'trackerName', 'deleteTracker_trackerName' - The name of the tracker resource to be deleted.
newDeleteTracker ::
  -- | 'trackerName'
  Prelude.Text ->
  DeleteTracker
newDeleteTracker pTrackerName_ =
  DeleteTracker' {trackerName = pTrackerName_}

-- | The name of the tracker resource to be deleted.
deleteTracker_trackerName :: Lens.Lens' DeleteTracker Prelude.Text
deleteTracker_trackerName = Lens.lens (\DeleteTracker' {trackerName} -> trackerName) (\s@DeleteTracker' {} a -> s {trackerName = a} :: DeleteTracker)

instance Core.AWSRequest DeleteTracker where
  type
    AWSResponse DeleteTracker =
      DeleteTrackerResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteTrackerResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteTracker where
  hashWithSalt _salt DeleteTracker' {..} =
    _salt `Prelude.hashWithSalt` trackerName

instance Prelude.NFData DeleteTracker where
  rnf DeleteTracker' {..} = Prelude.rnf trackerName

instance Data.ToHeaders DeleteTracker where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteTracker where
  toPath DeleteTracker' {..} =
    Prelude.mconcat
      ["/tracking/v0/trackers/", Data.toBS trackerName]

instance Data.ToQuery DeleteTracker where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteTrackerResponse' smart constructor.
data DeleteTrackerResponse = DeleteTrackerResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteTrackerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteTrackerResponse_httpStatus' - The response's http status code.
newDeleteTrackerResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteTrackerResponse
newDeleteTrackerResponse pHttpStatus_ =
  DeleteTrackerResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteTrackerResponse_httpStatus :: Lens.Lens' DeleteTrackerResponse Prelude.Int
deleteTrackerResponse_httpStatus = Lens.lens (\DeleteTrackerResponse' {httpStatus} -> httpStatus) (\s@DeleteTrackerResponse' {} a -> s {httpStatus = a} :: DeleteTrackerResponse)

instance Prelude.NFData DeleteTrackerResponse where
  rnf DeleteTrackerResponse' {..} =
    Prelude.rnf httpStatus
