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
-- Module      : Amazonka.SSMIncidents.DeleteReplicationSet
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes all Regions in your replication set. Deleting the replication
-- set deletes all Incident Manager data.
module Amazonka.SSMIncidents.DeleteReplicationSet
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSMIncidents.Types

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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteReplicationSetResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteReplicationSet where
  hashWithSalt _salt DeleteReplicationSet' {..} =
    _salt `Prelude.hashWithSalt` arn

instance Prelude.NFData DeleteReplicationSet where
  rnf DeleteReplicationSet' {..} = Prelude.rnf arn

instance Data.ToHeaders DeleteReplicationSet where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteReplicationSet where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath DeleteReplicationSet where
  toPath = Prelude.const "/deleteReplicationSet"

instance Data.ToQuery DeleteReplicationSet where
  toQuery DeleteReplicationSet' {..} =
    Prelude.mconcat ["arn" Data.=: arn]

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

instance Prelude.NFData DeleteReplicationSetResponse where
  rnf DeleteReplicationSetResponse' {..} =
    Prelude.rnf httpStatus
