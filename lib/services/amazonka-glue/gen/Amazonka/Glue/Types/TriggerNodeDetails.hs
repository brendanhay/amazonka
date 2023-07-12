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
-- Module      : Amazonka.Glue.Types.TriggerNodeDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.TriggerNodeDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types.Trigger
import qualified Amazonka.Prelude as Prelude

-- | The details of a Trigger node present in the workflow.
--
-- /See:/ 'newTriggerNodeDetails' smart constructor.
data TriggerNodeDetails = TriggerNodeDetails'
  { -- | The information of the trigger represented by the trigger node.
    trigger :: Prelude.Maybe Trigger
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TriggerNodeDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'trigger', 'triggerNodeDetails_trigger' - The information of the trigger represented by the trigger node.
newTriggerNodeDetails ::
  TriggerNodeDetails
newTriggerNodeDetails =
  TriggerNodeDetails' {trigger = Prelude.Nothing}

-- | The information of the trigger represented by the trigger node.
triggerNodeDetails_trigger :: Lens.Lens' TriggerNodeDetails (Prelude.Maybe Trigger)
triggerNodeDetails_trigger = Lens.lens (\TriggerNodeDetails' {trigger} -> trigger) (\s@TriggerNodeDetails' {} a -> s {trigger = a} :: TriggerNodeDetails)

instance Data.FromJSON TriggerNodeDetails where
  parseJSON =
    Data.withObject
      "TriggerNodeDetails"
      ( \x ->
          TriggerNodeDetails'
            Prelude.<$> (x Data..:? "Trigger")
      )

instance Prelude.Hashable TriggerNodeDetails where
  hashWithSalt _salt TriggerNodeDetails' {..} =
    _salt `Prelude.hashWithSalt` trigger

instance Prelude.NFData TriggerNodeDetails where
  rnf TriggerNodeDetails' {..} = Prelude.rnf trigger
