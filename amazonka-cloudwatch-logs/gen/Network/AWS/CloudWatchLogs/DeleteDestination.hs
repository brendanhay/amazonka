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
-- Module      : Network.AWS.CloudWatchLogs.DeleteDestination
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified destination, and eventually disables all the
-- subscription filters that publish to it. This operation does not delete
-- the physical resource encapsulated by the destination.
module Network.AWS.CloudWatchLogs.DeleteDestination
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

import Network.AWS.CloudWatchLogs.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteDestination' smart constructor.
data DeleteDestination = DeleteDestination'
  { -- | The name of the destination.
    destinationName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  DeleteDestination
newDeleteDestination pDestinationName_ =
  DeleteDestination'
    { destinationName =
        pDestinationName_
    }

-- | The name of the destination.
deleteDestination_destinationName :: Lens.Lens' DeleteDestination Core.Text
deleteDestination_destinationName = Lens.lens (\DeleteDestination' {destinationName} -> destinationName) (\s@DeleteDestination' {} a -> s {destinationName = a} :: DeleteDestination)

instance Core.AWSRequest DeleteDestination where
  type
    AWSResponse DeleteDestination =
      DeleteDestinationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull DeleteDestinationResponse'

instance Core.Hashable DeleteDestination

instance Core.NFData DeleteDestination

instance Core.ToHeaders DeleteDestination where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Logs_20140328.DeleteDestination" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteDestination where
  toJSON DeleteDestination' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("destinationName" Core..= destinationName)
          ]
      )

instance Core.ToPath DeleteDestination where
  toPath = Core.const "/"

instance Core.ToQuery DeleteDestination where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteDestinationResponse' smart constructor.
data DeleteDestinationResponse = DeleteDestinationResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteDestinationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteDestinationResponse ::
  DeleteDestinationResponse
newDeleteDestinationResponse =
  DeleteDestinationResponse'

instance Core.NFData DeleteDestinationResponse
