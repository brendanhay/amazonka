{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.ECR.Types.LifecyclePolicyRuleAction
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECR.Types.LifecyclePolicyRuleAction where

import Network.AWS.ECR.Types.ImageActionType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The type of action to be taken.
--
-- /See:/ 'newLifecyclePolicyRuleAction' smart constructor.
data LifecyclePolicyRuleAction = LifecyclePolicyRuleAction'
  { -- | The type of action to be taken.
    type' :: Prelude.Maybe ImageActionType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.FromJSON LifecyclePolicyRuleAction where
  parseJSON =
    Prelude.withObject
      "LifecyclePolicyRuleAction"
      ( \x ->
          LifecyclePolicyRuleAction'
            Prelude.<$> (x Prelude..:? "type")
      )

instance Prelude.Hashable LifecyclePolicyRuleAction

instance Prelude.NFData LifecyclePolicyRuleAction
