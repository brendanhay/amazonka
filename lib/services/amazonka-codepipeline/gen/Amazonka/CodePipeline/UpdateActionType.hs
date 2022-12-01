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
-- Module      : Amazonka.CodePipeline.UpdateActionType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an action type that was created with any supported integration
-- model, where the action type is to be used by customers of the action
-- type provider. Use a JSON file with the action definition and
-- @UpdateActionType@ to provide the full structure.
module Amazonka.CodePipeline.UpdateActionType
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

import Amazonka.CodePipeline.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateActionType' smart constructor.
data UpdateActionType = UpdateActionType'
  { -- | The action type definition for the action type to be updated.
    actionType :: ActionTypeDeclaration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  -- | 'actionType'
  ActionTypeDeclaration ->
  UpdateActionType
newUpdateActionType pActionType_ =
  UpdateActionType' {actionType = pActionType_}

-- | The action type definition for the action type to be updated.
updateActionType_actionType :: Lens.Lens' UpdateActionType ActionTypeDeclaration
updateActionType_actionType = Lens.lens (\UpdateActionType' {actionType} -> actionType) (\s@UpdateActionType' {} a -> s {actionType = a} :: UpdateActionType)

instance Core.AWSRequest UpdateActionType where
  type
    AWSResponse UpdateActionType =
      UpdateActionTypeResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull UpdateActionTypeResponse'

instance Prelude.Hashable UpdateActionType where
  hashWithSalt _salt UpdateActionType' {..} =
    _salt `Prelude.hashWithSalt` actionType

instance Prelude.NFData UpdateActionType where
  rnf UpdateActionType' {..} = Prelude.rnf actionType

instance Core.ToHeaders UpdateActionType where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodePipeline_20150709.UpdateActionType" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateActionType where
  toJSON UpdateActionType' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("actionType" Core..= actionType)]
      )

instance Core.ToPath UpdateActionType where
  toPath = Prelude.const "/"

instance Core.ToQuery UpdateActionType where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateActionTypeResponse' smart constructor.
data UpdateActionTypeResponse = UpdateActionTypeResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateActionTypeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUpdateActionTypeResponse ::
  UpdateActionTypeResponse
newUpdateActionTypeResponse =
  UpdateActionTypeResponse'

instance Prelude.NFData UpdateActionTypeResponse where
  rnf _ = ()
