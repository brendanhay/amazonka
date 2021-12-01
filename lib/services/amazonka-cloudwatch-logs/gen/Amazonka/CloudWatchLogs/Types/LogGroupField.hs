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
-- Module      : Amazonka.CloudWatchLogs.Types.LogGroupField
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudWatchLogs.Types.LogGroupField where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The fields contained in log events found by a @GetLogGroupFields@
-- operation, along with the percentage of queried log events in which each
-- field appears.
--
-- /See:/ 'newLogGroupField' smart constructor.
data LogGroupField = LogGroupField'
  { -- | The percentage of log events queried that contained the field.
    percent :: Prelude.Maybe Prelude.Natural,
    -- | The name of a log field.
    name :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LogGroupField' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'percent', 'logGroupField_percent' - The percentage of log events queried that contained the field.
--
-- 'name', 'logGroupField_name' - The name of a log field.
newLogGroupField ::
  LogGroupField
newLogGroupField =
  LogGroupField'
    { percent = Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | The percentage of log events queried that contained the field.
logGroupField_percent :: Lens.Lens' LogGroupField (Prelude.Maybe Prelude.Natural)
logGroupField_percent = Lens.lens (\LogGroupField' {percent} -> percent) (\s@LogGroupField' {} a -> s {percent = a} :: LogGroupField)

-- | The name of a log field.
logGroupField_name :: Lens.Lens' LogGroupField (Prelude.Maybe Prelude.Text)
logGroupField_name = Lens.lens (\LogGroupField' {name} -> name) (\s@LogGroupField' {} a -> s {name = a} :: LogGroupField)

instance Core.FromJSON LogGroupField where
  parseJSON =
    Core.withObject
      "LogGroupField"
      ( \x ->
          LogGroupField'
            Prelude.<$> (x Core..:? "percent")
            Prelude.<*> (x Core..:? "name")
      )

instance Prelude.Hashable LogGroupField where
  hashWithSalt salt' LogGroupField' {..} =
    salt' `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` percent

instance Prelude.NFData LogGroupField where
  rnf LogGroupField' {..} =
    Prelude.rnf percent `Prelude.seq` Prelude.rnf name
