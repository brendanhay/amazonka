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
-- Module      : Amazonka.TimeStreamQuery.DeleteScheduledQuery
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a given scheduled query. This is an irreversible operation.
module Amazonka.TimeStreamQuery.DeleteScheduledQuery
  ( -- * Creating a Request
    DeleteScheduledQuery (..),
    newDeleteScheduledQuery,

    -- * Request Lenses
    deleteScheduledQuery_scheduledQueryArn,

    -- * Destructuring the Response
    DeleteScheduledQueryResponse (..),
    newDeleteScheduledQueryResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.TimeStreamQuery.Types

-- | /See:/ 'newDeleteScheduledQuery' smart constructor.
data DeleteScheduledQuery = DeleteScheduledQuery'
  { -- | The ARN of the scheduled query.
    scheduledQueryArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteScheduledQuery' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'scheduledQueryArn', 'deleteScheduledQuery_scheduledQueryArn' - The ARN of the scheduled query.
newDeleteScheduledQuery ::
  -- | 'scheduledQueryArn'
  Prelude.Text ->
  DeleteScheduledQuery
newDeleteScheduledQuery pScheduledQueryArn_ =
  DeleteScheduledQuery'
    { scheduledQueryArn =
        pScheduledQueryArn_
    }

-- | The ARN of the scheduled query.
deleteScheduledQuery_scheduledQueryArn :: Lens.Lens' DeleteScheduledQuery Prelude.Text
deleteScheduledQuery_scheduledQueryArn = Lens.lens (\DeleteScheduledQuery' {scheduledQueryArn} -> scheduledQueryArn) (\s@DeleteScheduledQuery' {} a -> s {scheduledQueryArn = a} :: DeleteScheduledQuery)

instance Core.AWSRequest DeleteScheduledQuery where
  type
    AWSResponse DeleteScheduledQuery =
      DeleteScheduledQueryResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull DeleteScheduledQueryResponse'

instance Prelude.Hashable DeleteScheduledQuery where
  hashWithSalt _salt DeleteScheduledQuery' {..} =
    _salt `Prelude.hashWithSalt` scheduledQueryArn

instance Prelude.NFData DeleteScheduledQuery where
  rnf DeleteScheduledQuery' {..} =
    Prelude.rnf scheduledQueryArn

instance Data.ToHeaders DeleteScheduledQuery where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Timestream_20181101.DeleteScheduledQuery" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteScheduledQuery where
  toJSON DeleteScheduledQuery' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ScheduledQueryArn" Data..= scheduledQueryArn)
          ]
      )

instance Data.ToPath DeleteScheduledQuery where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteScheduledQuery where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteScheduledQueryResponse' smart constructor.
data DeleteScheduledQueryResponse = DeleteScheduledQueryResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteScheduledQueryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteScheduledQueryResponse ::
  DeleteScheduledQueryResponse
newDeleteScheduledQueryResponse =
  DeleteScheduledQueryResponse'

instance Prelude.NFData DeleteScheduledQueryResponse where
  rnf _ = ()
