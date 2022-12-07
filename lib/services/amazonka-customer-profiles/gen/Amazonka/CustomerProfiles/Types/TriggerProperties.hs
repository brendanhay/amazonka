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
-- Module      : Amazonka.CustomerProfiles.Types.TriggerProperties
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CustomerProfiles.Types.TriggerProperties where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.CustomerProfiles.Types.ScheduledTriggerProperties
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies the configuration details that control the trigger for a flow.
-- Currently, these settings only apply to the Scheduled trigger type.
--
-- /See:/ 'newTriggerProperties' smart constructor.
data TriggerProperties = TriggerProperties'
  { -- | Specifies the configuration details of a schedule-triggered flow that
    -- you define.
    scheduled :: Prelude.Maybe ScheduledTriggerProperties
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TriggerProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'scheduled', 'triggerProperties_scheduled' - Specifies the configuration details of a schedule-triggered flow that
-- you define.
newTriggerProperties ::
  TriggerProperties
newTriggerProperties =
  TriggerProperties' {scheduled = Prelude.Nothing}

-- | Specifies the configuration details of a schedule-triggered flow that
-- you define.
triggerProperties_scheduled :: Lens.Lens' TriggerProperties (Prelude.Maybe ScheduledTriggerProperties)
triggerProperties_scheduled = Lens.lens (\TriggerProperties' {scheduled} -> scheduled) (\s@TriggerProperties' {} a -> s {scheduled = a} :: TriggerProperties)

instance Prelude.Hashable TriggerProperties where
  hashWithSalt _salt TriggerProperties' {..} =
    _salt `Prelude.hashWithSalt` scheduled

instance Prelude.NFData TriggerProperties where
  rnf TriggerProperties' {..} = Prelude.rnf scheduled

instance Data.ToJSON TriggerProperties where
  toJSON TriggerProperties' {..} =
    Data.object
      ( Prelude.catMaybes
          [("Scheduled" Data..=) Prelude.<$> scheduled]
      )
