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
-- Module      : Amazonka.Budgets.Types.ActionThreshold
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Budgets.Types.ActionThreshold where

import Amazonka.Budgets.Types.ThresholdType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The trigger threshold of the action.
--
-- /See:/ 'newActionThreshold' smart constructor.
data ActionThreshold = ActionThreshold'
  { actionThresholdValue :: Prelude.Double,
    actionThresholdType :: ThresholdType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ActionThreshold' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'actionThresholdValue', 'actionThreshold_actionThresholdValue' - Undocumented member.
--
-- 'actionThresholdType', 'actionThreshold_actionThresholdType' - Undocumented member.
newActionThreshold ::
  -- | 'actionThresholdValue'
  Prelude.Double ->
  -- | 'actionThresholdType'
  ThresholdType ->
  ActionThreshold
newActionThreshold
  pActionThresholdValue_
  pActionThresholdType_ =
    ActionThreshold'
      { actionThresholdValue =
          pActionThresholdValue_,
        actionThresholdType = pActionThresholdType_
      }

-- | Undocumented member.
actionThreshold_actionThresholdValue :: Lens.Lens' ActionThreshold Prelude.Double
actionThreshold_actionThresholdValue = Lens.lens (\ActionThreshold' {actionThresholdValue} -> actionThresholdValue) (\s@ActionThreshold' {} a -> s {actionThresholdValue = a} :: ActionThreshold)

-- | Undocumented member.
actionThreshold_actionThresholdType :: Lens.Lens' ActionThreshold ThresholdType
actionThreshold_actionThresholdType = Lens.lens (\ActionThreshold' {actionThresholdType} -> actionThresholdType) (\s@ActionThreshold' {} a -> s {actionThresholdType = a} :: ActionThreshold)

instance Data.FromJSON ActionThreshold where
  parseJSON =
    Data.withObject
      "ActionThreshold"
      ( \x ->
          ActionThreshold'
            Prelude.<$> (x Data..: "ActionThresholdValue")
            Prelude.<*> (x Data..: "ActionThresholdType")
      )

instance Prelude.Hashable ActionThreshold where
  hashWithSalt _salt ActionThreshold' {..} =
    _salt `Prelude.hashWithSalt` actionThresholdValue
      `Prelude.hashWithSalt` actionThresholdType

instance Prelude.NFData ActionThreshold where
  rnf ActionThreshold' {..} =
    Prelude.rnf actionThresholdValue
      `Prelude.seq` Prelude.rnf actionThresholdType

instance Data.ToJSON ActionThreshold where
  toJSON ActionThreshold' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "ActionThresholdValue"
                  Data..= actionThresholdValue
              ),
            Prelude.Just
              ("ActionThresholdType" Data..= actionThresholdType)
          ]
      )
