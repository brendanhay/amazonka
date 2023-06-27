{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.VerifiedPermissions.Types.ActionIdentifier
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.VerifiedPermissions.Types.ActionIdentifier where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information about an action for a request for which an
-- authorization decision is made.
--
-- This data type is used as an request parameter to the
-- <https://docs.aws.amazon.com/verifiedpermissions/latest/apireference/API_IsAuthorized.html IsAuthorized>
-- and
-- <https://docs.aws.amazon.com/verifiedpermissions/latest/apireference/API_IsAuthorizedWithToken.html IsAuthorizedWithToken>
-- operations.
--
-- Example:
-- @{ \"actionId\": \"\<action name>\", \"actionType\": \"Action\" }@
--
-- /See:/ 'newActionIdentifier' smart constructor.
data ActionIdentifier = ActionIdentifier'
  { -- | The type of an action.
    actionType :: Prelude.Text,
    -- | The ID of an action.
    actionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ActionIdentifier' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'actionType', 'actionIdentifier_actionType' - The type of an action.
--
-- 'actionId', 'actionIdentifier_actionId' - The ID of an action.
newActionIdentifier ::
  -- | 'actionType'
  Prelude.Text ->
  -- | 'actionId'
  Prelude.Text ->
  ActionIdentifier
newActionIdentifier pActionType_ pActionId_ =
  ActionIdentifier'
    { actionType = pActionType_,
      actionId = pActionId_
    }

-- | The type of an action.
actionIdentifier_actionType :: Lens.Lens' ActionIdentifier Prelude.Text
actionIdentifier_actionType = Lens.lens (\ActionIdentifier' {actionType} -> actionType) (\s@ActionIdentifier' {} a -> s {actionType = a} :: ActionIdentifier)

-- | The ID of an action.
actionIdentifier_actionId :: Lens.Lens' ActionIdentifier Prelude.Text
actionIdentifier_actionId = Lens.lens (\ActionIdentifier' {actionId} -> actionId) (\s@ActionIdentifier' {} a -> s {actionId = a} :: ActionIdentifier)

instance Prelude.Hashable ActionIdentifier where
  hashWithSalt _salt ActionIdentifier' {..} =
    _salt
      `Prelude.hashWithSalt` actionType
      `Prelude.hashWithSalt` actionId

instance Prelude.NFData ActionIdentifier where
  rnf ActionIdentifier' {..} =
    Prelude.rnf actionType
      `Prelude.seq` Prelude.rnf actionId

instance Data.ToJSON ActionIdentifier where
  toJSON ActionIdentifier' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("actionType" Data..= actionType),
            Prelude.Just ("actionId" Data..= actionId)
          ]
      )
