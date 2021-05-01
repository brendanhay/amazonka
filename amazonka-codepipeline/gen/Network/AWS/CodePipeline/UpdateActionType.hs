{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.CodePipeline.UpdateActionType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an action type that was created with any supported integration
-- model, where the action type is to be used by customers of the action
-- type provider. Use a JSON file with the action definition and
-- @UpdateActionType@ to provide the full structure.
module Network.AWS.CodePipeline.UpdateActionType
  ( -- * Creating a Request
    UpdateActionType (..),
    newUpdateActionType,

    -- * Request Lenses
    updateActionType_actionType,

    -- * Destructuring the Response
    UpdateActionTypeResponse (..),
    newUpdateActionTypeResponse,
  )
where

import Network.AWS.CodePipeline.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateActionType' smart constructor.
data UpdateActionType = UpdateActionType'
  { -- | The action type definition for the action type to be updated.
    actionType :: Prelude.Maybe ActionTypeDeclaration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateActionType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'actionType', 'updateActionType_actionType' - The action type definition for the action type to be updated.
newUpdateActionType ::
  UpdateActionType
newUpdateActionType =
  UpdateActionType' {actionType = Prelude.Nothing}

-- | The action type definition for the action type to be updated.
updateActionType_actionType :: Lens.Lens' UpdateActionType (Prelude.Maybe ActionTypeDeclaration)
updateActionType_actionType = Lens.lens (\UpdateActionType' {actionType} -> actionType) (\s@UpdateActionType' {} a -> s {actionType = a} :: UpdateActionType)

instance Prelude.AWSRequest UpdateActionType where
  type Rs UpdateActionType = UpdateActionTypeResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull UpdateActionTypeResponse'

instance Prelude.Hashable UpdateActionType

instance Prelude.NFData UpdateActionType

instance Prelude.ToHeaders UpdateActionType where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "CodePipeline_20150709.UpdateActionType" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON UpdateActionType where
  toJSON UpdateActionType' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [("actionType" Prelude..=) Prelude.<$> actionType]
      )

instance Prelude.ToPath UpdateActionType where
  toPath = Prelude.const "/"

instance Prelude.ToQuery UpdateActionType where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateActionTypeResponse' smart constructor.
data UpdateActionTypeResponse = UpdateActionTypeResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateActionTypeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUpdateActionTypeResponse ::
  UpdateActionTypeResponse
newUpdateActionTypeResponse =
  UpdateActionTypeResponse'

instance Prelude.NFData UpdateActionTypeResponse
