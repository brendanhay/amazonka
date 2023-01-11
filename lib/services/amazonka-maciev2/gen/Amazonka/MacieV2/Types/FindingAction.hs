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
-- Module      : Amazonka.MacieV2.Types.FindingAction
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.FindingAction where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MacieV2.Types.ApiCallDetails
import Amazonka.MacieV2.Types.FindingActionType
import qualified Amazonka.Prelude as Prelude

-- | Provides information about an action that occurred for a resource and
-- produced a policy finding.
--
-- /See:/ 'newFindingAction' smart constructor.
data FindingAction = FindingAction'
  { -- | The type of action that occurred for the affected resource. This value
    -- is typically AWS_API_CALL, which indicates that an entity invoked an API
    -- operation for the resource.
    actionType :: Prelude.Maybe FindingActionType,
    -- | The invocation details of the API operation that an entity invoked for
    -- the affected resource, if the value for the actionType property is
    -- AWS_API_CALL.
    apiCallDetails :: Prelude.Maybe ApiCallDetails
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
-- 'actionType', 'findingAction_actionType' - The type of action that occurred for the affected resource. This value
-- is typically AWS_API_CALL, which indicates that an entity invoked an API
-- operation for the resource.
--
-- 'apiCallDetails', 'findingAction_apiCallDetails' - The invocation details of the API operation that an entity invoked for
-- the affected resource, if the value for the actionType property is
-- AWS_API_CALL.
newFindingAction ::
  FindingAction
newFindingAction =
  FindingAction'
    { actionType = Prelude.Nothing,
      apiCallDetails = Prelude.Nothing
    }

-- | The type of action that occurred for the affected resource. This value
-- is typically AWS_API_CALL, which indicates that an entity invoked an API
-- operation for the resource.
findingAction_actionType :: Lens.Lens' FindingAction (Prelude.Maybe FindingActionType)
findingAction_actionType = Lens.lens (\FindingAction' {actionType} -> actionType) (\s@FindingAction' {} a -> s {actionType = a} :: FindingAction)

-- | The invocation details of the API operation that an entity invoked for
-- the affected resource, if the value for the actionType property is
-- AWS_API_CALL.
findingAction_apiCallDetails :: Lens.Lens' FindingAction (Prelude.Maybe ApiCallDetails)
findingAction_apiCallDetails = Lens.lens (\FindingAction' {apiCallDetails} -> apiCallDetails) (\s@FindingAction' {} a -> s {apiCallDetails = a} :: FindingAction)

instance Data.FromJSON FindingAction where
  parseJSON =
    Data.withObject
      "FindingAction"
      ( \x ->
          FindingAction'
            Prelude.<$> (x Data..:? "actionType")
            Prelude.<*> (x Data..:? "apiCallDetails")
      )

instance Prelude.Hashable FindingAction where
  hashWithSalt _salt FindingAction' {..} =
    _salt `Prelude.hashWithSalt` actionType
      `Prelude.hashWithSalt` apiCallDetails

instance Prelude.NFData FindingAction where
  rnf FindingAction' {..} =
    Prelude.rnf actionType
      `Prelude.seq` Prelude.rnf apiCallDetails
