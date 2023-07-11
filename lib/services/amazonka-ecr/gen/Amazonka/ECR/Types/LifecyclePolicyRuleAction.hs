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
-- Module      : Amazonka.ECR.Types.LifecyclePolicyRuleAction
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ECR.Types.LifecyclePolicyRuleAction where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ECR.Types.ImageActionType
import qualified Amazonka.Prelude as Prelude

-- | The type of action to be taken.
--
-- /See:/ 'newLifecyclePolicyRuleAction' smart constructor.
data LifecyclePolicyRuleAction = LifecyclePolicyRuleAction'
  { -- | The type of action to be taken.
    type' :: Prelude.Maybe ImageActionType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LifecyclePolicyRuleAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'type'', 'lifecyclePolicyRuleAction_type' - The type of action to be taken.
newLifecyclePolicyRuleAction ::
  LifecyclePolicyRuleAction
newLifecyclePolicyRuleAction =
  LifecyclePolicyRuleAction' {type' = Prelude.Nothing}

-- | The type of action to be taken.
lifecyclePolicyRuleAction_type :: Lens.Lens' LifecyclePolicyRuleAction (Prelude.Maybe ImageActionType)
lifecyclePolicyRuleAction_type = Lens.lens (\LifecyclePolicyRuleAction' {type'} -> type') (\s@LifecyclePolicyRuleAction' {} a -> s {type' = a} :: LifecyclePolicyRuleAction)

instance Data.FromJSON LifecyclePolicyRuleAction where
  parseJSON =
    Data.withObject
      "LifecyclePolicyRuleAction"
      ( \x ->
          LifecyclePolicyRuleAction'
            Prelude.<$> (x Data..:? "type")
      )

instance Prelude.Hashable LifecyclePolicyRuleAction where
  hashWithSalt _salt LifecyclePolicyRuleAction' {..} =
    _salt `Prelude.hashWithSalt` type'

instance Prelude.NFData LifecyclePolicyRuleAction where
  rnf LifecyclePolicyRuleAction' {..} =
    Prelude.rnf type'
