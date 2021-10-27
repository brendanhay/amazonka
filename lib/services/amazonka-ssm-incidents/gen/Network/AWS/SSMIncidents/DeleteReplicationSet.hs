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
-- Module      : Network.AWS.SSMIncidents.DeleteReplicationSet
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes all Regions in your replication set. Deleting the replication
-- set deletes all Incident Manager data.
module Network.AWS.SSMIncidents.DeleteReplicationSet
  ( -- * Creating a Request
    DeleteReplicationSet (..),
    newDeleteReplicationSet,

    -- * Request Lenses
    deleteReplicationSet_arn,

    -- * Destructuring the Response
    DeleteReplicationSetResponse (..),
    newDeleteReplicationSetResponse,

    -- * Response Lenses
    deleteReplicationSetResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSMIncidents.Types

-- | /See:/ 'newDeleteReplicationSet' smart constructor.
data DeleteReplicationSet = DeleteReplicationSet'
  { -- | The Amazon Resource Name (ARN) of the replication set you\'re deleting.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteReplicationSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'deleteReplicationSet_arn' - The Amazon Resource Name (ARN) of the replication set you\'re deleting.
newDeleteReplicationSet ::
  -- | 'arn'
  Prelude.Text ->
  DeleteReplicationSet
newDeleteReplicationSet pArn_ =
  DeleteReplicationSet' {arn = pArn_}

-- | The Amazon Resource Name (ARN) of the replication set you\'re deleting.
deleteReplicationSet_arn :: Lens.Lens' DeleteReplicationSet Prelude.Text
deleteReplicationSet_arn = Lens.lens (\DeleteReplicationSet' {arn} -> arn) (\s@DeleteReplicationSet' {} a -> s {arn = a} :: DeleteReplicationSet)

instance Core.AWSRequest DeleteReplicationSet where
  type
    AWSResponse DeleteReplicationSet =
      DeleteReplicationSetResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteReplicationSetResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteReplicationSet

instance Prelude.NFData DeleteReplicationSet

instance Core.ToHeaders DeleteReplicationSet where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DeleteReplicationSet where
  toJSON = Prelude.const (Core.Object Prelude.mempty)

instance Core.ToPath DeleteReplicationSet where
  toPath = Prelude.const "/deleteReplicationSet"

instance Core.ToQuery DeleteReplicationSet where
  toQuery DeleteReplicationSet' {..} =
    Prelude.mconcat ["arn" Core.=: arn]

-- | /See:/ 'newDeleteReplicationSetResponse' smart constructor.
data DeleteReplicationSetResponse = DeleteReplicationSetResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteReplicationSetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteReplicationSetResponse_httpStatus' - The response's http status code.
newDeleteReplicationSetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteReplicationSetResponse
newDeleteReplicationSetResponse pHttpStatus_ =
  DeleteReplicationSetResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteReplicationSetResponse_httpStatus :: Lens.Lens' DeleteReplicationSetResponse Prelude.Int
deleteReplicationSetResponse_httpStatus = Lens.lens (\DeleteReplicationSetResponse' {httpStatus} -> httpStatus) (\s@DeleteReplicationSetResponse' {} a -> s {httpStatus = a} :: DeleteReplicationSetResponse)

instance Prelude.NFData DeleteReplicationSetResponse
