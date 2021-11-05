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
-- Module      : Network.AWS.MacieV2.Types.FindingAction
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MacieV2.Types.FindingAction where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MacieV2.Types.ApiCallDetails
import Network.AWS.MacieV2.Types.FindingActionType
import qualified Network.AWS.Prelude as Prelude

-- | Provides information about an action that occurred for a resource and
-- produced a policy finding.
--
-- /See:/ 'newFindingAction' smart constructor.
data FindingAction = FindingAction'
  { -- | The invocation details of the API operation that an entity invoked for
    -- the affected resource, if the value for the actionType property is
    -- AWS_API_CALL.
    apiCallDetails :: Prelude.Maybe ApiCallDetails,
    -- | The type of action that occurred for the affected resource. This value
    -- is typically AWS_API_CALL, which indicates that an entity invoked an API
    -- operation for the resource.
    actionType :: Prelude.Maybe FindingActionType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FindingAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'apiCallDetails', 'findingAction_apiCallDetails' - The invocation details of the API operation that an entity invoked for
-- the affected resource, if the value for the actionType property is
-- AWS_API_CALL.
--
-- 'actionType', 'findingAction_actionType' - The type of action that occurred for the affected resource. This value
-- is typically AWS_API_CALL, which indicates that an entity invoked an API
-- operation for the resource.
newFindingAction ::
  FindingAction
newFindingAction =
  FindingAction'
    { apiCallDetails = Prelude.Nothing,
      actionType = Prelude.Nothing
    }

-- | The invocation details of the API operation that an entity invoked for
-- the affected resource, if the value for the actionType property is
-- AWS_API_CALL.
findingAction_apiCallDetails :: Lens.Lens' FindingAction (Prelude.Maybe ApiCallDetails)
findingAction_apiCallDetails = Lens.lens (\FindingAction' {apiCallDetails} -> apiCallDetails) (\s@FindingAction' {} a -> s {apiCallDetails = a} :: FindingAction)

-- | The type of action that occurred for the affected resource. This value
-- is typically AWS_API_CALL, which indicates that an entity invoked an API
-- operation for the resource.
findingAction_actionType :: Lens.Lens' FindingAction (Prelude.Maybe FindingActionType)
findingAction_actionType = Lens.lens (\FindingAction' {actionType} -> actionType) (\s@FindingAction' {} a -> s {actionType = a} :: FindingAction)

instance Core.FromJSON FindingAction where
  parseJSON =
    Core.withObject
      "FindingAction"
      ( \x ->
          FindingAction'
            Prelude.<$> (x Core..:? "apiCallDetails")
            Prelude.<*> (x Core..:? "actionType")
      )

instance Prelude.Hashable FindingAction

instance Prelude.NFData FindingAction
