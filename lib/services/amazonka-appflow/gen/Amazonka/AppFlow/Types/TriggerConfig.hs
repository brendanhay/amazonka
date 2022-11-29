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
-- Module      : Amazonka.AppFlow.Types.TriggerConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Types.TriggerConfig where

import Amazonka.AppFlow.Types.TriggerProperties
import Amazonka.AppFlow.Types.TriggerType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The trigger settings that determine how and when Amazon AppFlow runs the
-- specified flow.
--
-- /See:/ 'newTriggerConfig' smart constructor.
data TriggerConfig = TriggerConfig'
  { -- | Specifies the configuration details of a schedule-triggered flow as
    -- defined by the user. Currently, these settings only apply to the
    -- @Scheduled@ trigger type.
    triggerProperties :: Prelude.Maybe TriggerProperties,
    -- | Specifies the type of flow trigger. This can be @OnDemand@, @Scheduled@,
    -- or @Event@.
    triggerType :: TriggerType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TriggerConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'triggerProperties', 'triggerConfig_triggerProperties' - Specifies the configuration details of a schedule-triggered flow as
-- defined by the user. Currently, these settings only apply to the
-- @Scheduled@ trigger type.
--
-- 'triggerType', 'triggerConfig_triggerType' - Specifies the type of flow trigger. This can be @OnDemand@, @Scheduled@,
-- or @Event@.
newTriggerConfig ::
  -- | 'triggerType'
  TriggerType ->
  TriggerConfig
newTriggerConfig pTriggerType_ =
  TriggerConfig'
    { triggerProperties = Prelude.Nothing,
      triggerType = pTriggerType_
    }

-- | Specifies the configuration details of a schedule-triggered flow as
-- defined by the user. Currently, these settings only apply to the
-- @Scheduled@ trigger type.
triggerConfig_triggerProperties :: Lens.Lens' TriggerConfig (Prelude.Maybe TriggerProperties)
triggerConfig_triggerProperties = Lens.lens (\TriggerConfig' {triggerProperties} -> triggerProperties) (\s@TriggerConfig' {} a -> s {triggerProperties = a} :: TriggerConfig)

-- | Specifies the type of flow trigger. This can be @OnDemand@, @Scheduled@,
-- or @Event@.
triggerConfig_triggerType :: Lens.Lens' TriggerConfig TriggerType
triggerConfig_triggerType = Lens.lens (\TriggerConfig' {triggerType} -> triggerType) (\s@TriggerConfig' {} a -> s {triggerType = a} :: TriggerConfig)

instance Core.FromJSON TriggerConfig where
  parseJSON =
    Core.withObject
      "TriggerConfig"
      ( \x ->
          TriggerConfig'
            Prelude.<$> (x Core..:? "triggerProperties")
            Prelude.<*> (x Core..: "triggerType")
      )

instance Prelude.Hashable TriggerConfig where
  hashWithSalt _salt TriggerConfig' {..} =
    _salt `Prelude.hashWithSalt` triggerProperties
      `Prelude.hashWithSalt` triggerType

instance Prelude.NFData TriggerConfig where
  rnf TriggerConfig' {..} =
    Prelude.rnf triggerProperties
      `Prelude.seq` Prelude.rnf triggerType

instance Core.ToJSON TriggerConfig where
  toJSON TriggerConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("triggerProperties" Core..=)
              Prelude.<$> triggerProperties,
            Prelude.Just ("triggerType" Core..= triggerType)
          ]
      )
