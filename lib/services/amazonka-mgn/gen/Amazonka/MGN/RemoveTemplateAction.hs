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
-- Module      : Amazonka.MGN.RemoveTemplateAction
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Remove template post migration custom action.
module Amazonka.MGN.RemoveTemplateAction
  ( -- * Creating a Request
    RemoveTemplateAction (..),
    newRemoveTemplateAction,

    -- * Request Lenses
    removeTemplateAction_actionID,
    removeTemplateAction_launchConfigurationTemplateID,

    -- * Destructuring the Response
    RemoveTemplateActionResponse (..),
    newRemoveTemplateActionResponse,

    -- * Response Lenses
    removeTemplateActionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MGN.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newRemoveTemplateAction' smart constructor.
data RemoveTemplateAction = RemoveTemplateAction'
  { -- | Template post migration custom action ID to remove.
    actionID :: Prelude.Text,
    -- | Launch configuration template ID of the post migration custom action to
    -- remove.
    launchConfigurationTemplateID :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RemoveTemplateAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'actionID', 'removeTemplateAction_actionID' - Template post migration custom action ID to remove.
--
-- 'launchConfigurationTemplateID', 'removeTemplateAction_launchConfigurationTemplateID' - Launch configuration template ID of the post migration custom action to
-- remove.
newRemoveTemplateAction ::
  -- | 'actionID'
  Prelude.Text ->
  -- | 'launchConfigurationTemplateID'
  Prelude.Text ->
  RemoveTemplateAction
newRemoveTemplateAction
  pActionID_
  pLaunchConfigurationTemplateID_ =
    RemoveTemplateAction'
      { actionID = pActionID_,
        launchConfigurationTemplateID =
          pLaunchConfigurationTemplateID_
      }

-- | Template post migration custom action ID to remove.
removeTemplateAction_actionID :: Lens.Lens' RemoveTemplateAction Prelude.Text
removeTemplateAction_actionID = Lens.lens (\RemoveTemplateAction' {actionID} -> actionID) (\s@RemoveTemplateAction' {} a -> s {actionID = a} :: RemoveTemplateAction)

-- | Launch configuration template ID of the post migration custom action to
-- remove.
removeTemplateAction_launchConfigurationTemplateID :: Lens.Lens' RemoveTemplateAction Prelude.Text
removeTemplateAction_launchConfigurationTemplateID = Lens.lens (\RemoveTemplateAction' {launchConfigurationTemplateID} -> launchConfigurationTemplateID) (\s@RemoveTemplateAction' {} a -> s {launchConfigurationTemplateID = a} :: RemoveTemplateAction)

instance Core.AWSRequest RemoveTemplateAction where
  type
    AWSResponse RemoveTemplateAction =
      RemoveTemplateActionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          RemoveTemplateActionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RemoveTemplateAction where
  hashWithSalt _salt RemoveTemplateAction' {..} =
    _salt
      `Prelude.hashWithSalt` actionID
      `Prelude.hashWithSalt` launchConfigurationTemplateID

instance Prelude.NFData RemoveTemplateAction where
  rnf RemoveTemplateAction' {..} =
    Prelude.rnf actionID `Prelude.seq`
      Prelude.rnf launchConfigurationTemplateID

instance Data.ToHeaders RemoveTemplateAction where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON RemoveTemplateAction where
  toJSON RemoveTemplateAction' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("actionID" Data..= actionID),
            Prelude.Just
              ( "launchConfigurationTemplateID"
                  Data..= launchConfigurationTemplateID
              )
          ]
      )

instance Data.ToPath RemoveTemplateAction where
  toPath = Prelude.const "/RemoveTemplateAction"

instance Data.ToQuery RemoveTemplateAction where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRemoveTemplateActionResponse' smart constructor.
data RemoveTemplateActionResponse = RemoveTemplateActionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RemoveTemplateActionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'removeTemplateActionResponse_httpStatus' - The response's http status code.
newRemoveTemplateActionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RemoveTemplateActionResponse
newRemoveTemplateActionResponse pHttpStatus_ =
  RemoveTemplateActionResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
removeTemplateActionResponse_httpStatus :: Lens.Lens' RemoveTemplateActionResponse Prelude.Int
removeTemplateActionResponse_httpStatus = Lens.lens (\RemoveTemplateActionResponse' {httpStatus} -> httpStatus) (\s@RemoveTemplateActionResponse' {} a -> s {httpStatus = a} :: RemoveTemplateActionResponse)

instance Prelude.NFData RemoveTemplateActionResponse where
  rnf RemoveTemplateActionResponse' {..} =
    Prelude.rnf httpStatus
