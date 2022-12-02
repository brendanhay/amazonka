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
-- Module      : Amazonka.CloudWatchLogs.DeleteDestination
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified destination, and eventually disables all the
-- subscription filters that publish to it. This operation does not delete
-- the physical resource encapsulated by the destination.
module Amazonka.CloudWatchLogs.DeleteDestination
  ( -- * Creating a Request
    DeleteDestination (..),
    newDeleteDestination,

    -- * Request Lenses
    deleteDestination_destinationName,

    -- * Destructuring the Response
    DeleteDestinationResponse (..),
    newDeleteDestinationResponse,
  )
where

import Amazonka.CloudWatchLogs.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteDestination' smart constructor.
data DeleteDestination = DeleteDestination'
  { -- | The name of the destination.
    destinationName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteDestination' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'destinationName', 'deleteDestination_destinationName' - The name of the destination.
newDeleteDestination ::
  -- | 'destinationName'
  Prelude.Text ->
  DeleteDestination
newDeleteDestination pDestinationName_ =
  DeleteDestination'
    { destinationName =
        pDestinationName_
    }

-- | The name of the destination.
deleteDestination_destinationName :: Lens.Lens' DeleteDestination Prelude.Text
deleteDestination_destinationName = Lens.lens (\DeleteDestination' {destinationName} -> destinationName) (\s@DeleteDestination' {} a -> s {destinationName = a} :: DeleteDestination)

instance Core.AWSRequest DeleteDestination where
  type
    AWSResponse DeleteDestination =
      DeleteDestinationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull DeleteDestinationResponse'

instance Prelude.Hashable DeleteDestination where
  hashWithSalt _salt DeleteDestination' {..} =
    _salt `Prelude.hashWithSalt` destinationName

instance Prelude.NFData DeleteDestination where
  rnf DeleteDestination' {..} =
    Prelude.rnf destinationName

instance Data.ToHeaders DeleteDestination where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Logs_20140328.DeleteDestination" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteDestination where
  toJSON DeleteDestination' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("destinationName" Data..= destinationName)
          ]
      )

instance Data.ToPath DeleteDestination where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteDestination where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteDestinationResponse' smart constructor.
data DeleteDestinationResponse = DeleteDestinationResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteDestinationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteDestinationResponse ::
  DeleteDestinationResponse
newDeleteDestinationResponse =
  DeleteDestinationResponse'

instance Prelude.NFData DeleteDestinationResponse where
  rnf _ = ()
