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
-- Module      : Amazonka.MGN.RemoveSourceServerAction
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Remove source server post migration custom action.
module Amazonka.MGN.RemoveSourceServerAction
  ( -- * Creating a Request
    RemoveSourceServerAction (..),
    newRemoveSourceServerAction,

    -- * Request Lenses
    removeSourceServerAction_actionID,
    removeSourceServerAction_sourceServerID,

    -- * Destructuring the Response
    RemoveSourceServerActionResponse (..),
    newRemoveSourceServerActionResponse,

    -- * Response Lenses
    removeSourceServerActionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MGN.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newRemoveSourceServerAction' smart constructor.
data RemoveSourceServerAction = RemoveSourceServerAction'
  { -- | Source server post migration custom action ID to remove.
    actionID :: Prelude.Text,
    -- | Source server ID of the post migration custom action to remove.
    sourceServerID :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RemoveSourceServerAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'actionID', 'removeSourceServerAction_actionID' - Source server post migration custom action ID to remove.
--
-- 'sourceServerID', 'removeSourceServerAction_sourceServerID' - Source server ID of the post migration custom action to remove.
newRemoveSourceServerAction ::
  -- | 'actionID'
  Prelude.Text ->
  -- | 'sourceServerID'
  Prelude.Text ->
  RemoveSourceServerAction
newRemoveSourceServerAction
  pActionID_
  pSourceServerID_ =
    RemoveSourceServerAction'
      { actionID = pActionID_,
        sourceServerID = pSourceServerID_
      }

-- | Source server post migration custom action ID to remove.
removeSourceServerAction_actionID :: Lens.Lens' RemoveSourceServerAction Prelude.Text
removeSourceServerAction_actionID = Lens.lens (\RemoveSourceServerAction' {actionID} -> actionID) (\s@RemoveSourceServerAction' {} a -> s {actionID = a} :: RemoveSourceServerAction)

-- | Source server ID of the post migration custom action to remove.
removeSourceServerAction_sourceServerID :: Lens.Lens' RemoveSourceServerAction Prelude.Text
removeSourceServerAction_sourceServerID = Lens.lens (\RemoveSourceServerAction' {sourceServerID} -> sourceServerID) (\s@RemoveSourceServerAction' {} a -> s {sourceServerID = a} :: RemoveSourceServerAction)

instance Core.AWSRequest RemoveSourceServerAction where
  type
    AWSResponse RemoveSourceServerAction =
      RemoveSourceServerActionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          RemoveSourceServerActionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RemoveSourceServerAction where
  hashWithSalt _salt RemoveSourceServerAction' {..} =
    _salt `Prelude.hashWithSalt` actionID
      `Prelude.hashWithSalt` sourceServerID

instance Prelude.NFData RemoveSourceServerAction where
  rnf RemoveSourceServerAction' {..} =
    Prelude.rnf actionID
      `Prelude.seq` Prelude.rnf sourceServerID

instance Data.ToHeaders RemoveSourceServerAction where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON RemoveSourceServerAction where
  toJSON RemoveSourceServerAction' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("actionID" Data..= actionID),
            Prelude.Just
              ("sourceServerID" Data..= sourceServerID)
          ]
      )

instance Data.ToPath RemoveSourceServerAction where
  toPath = Prelude.const "/RemoveSourceServerAction"

instance Data.ToQuery RemoveSourceServerAction where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRemoveSourceServerActionResponse' smart constructor.
data RemoveSourceServerActionResponse = RemoveSourceServerActionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RemoveSourceServerActionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'removeSourceServerActionResponse_httpStatus' - The response's http status code.
newRemoveSourceServerActionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RemoveSourceServerActionResponse
newRemoveSourceServerActionResponse pHttpStatus_ =
  RemoveSourceServerActionResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
removeSourceServerActionResponse_httpStatus :: Lens.Lens' RemoveSourceServerActionResponse Prelude.Int
removeSourceServerActionResponse_httpStatus = Lens.lens (\RemoveSourceServerActionResponse' {httpStatus} -> httpStatus) (\s@RemoveSourceServerActionResponse' {} a -> s {httpStatus = a} :: RemoveSourceServerActionResponse)

instance
  Prelude.NFData
    RemoveSourceServerActionResponse
  where
  rnf RemoveSourceServerActionResponse' {..} =
    Prelude.rnf httpStatus
