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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudWatchLogs.Types.LogGroupField where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The fields contained in log events found by a @GetLogGroupFields@
-- operation, along with the percentage of queried log events in which each
-- field appears.
--
-- /See:/ 'newLogGroupField' smart constructor.
data LogGroupField = LogGroupField'
  { -- | The name of a log field.
    name :: Prelude.Maybe Prelude.Text,
    -- | The percentage of log events queried that contained the field.
    percent :: Prelude.Maybe Prelude.Natural
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
-- 'name', 'logGroupField_name' - The name of a log field.
--
-- 'percent', 'logGroupField_percent' - The percentage of log events queried that contained the field.
newLogGroupField ::
  LogGroupField
newLogGroupField =
  LogGroupField'
    { name = Prelude.Nothing,
      percent = Prelude.Nothing
    }

-- | The name of a log field.
logGroupField_name :: Lens.Lens' LogGroupField (Prelude.Maybe Prelude.Text)
logGroupField_name = Lens.lens (\LogGroupField' {name} -> name) (\s@LogGroupField' {} a -> s {name = a} :: LogGroupField)

-- | The percentage of log events queried that contained the field.
logGroupField_percent :: Lens.Lens' LogGroupField (Prelude.Maybe Prelude.Natural)
logGroupField_percent = Lens.lens (\LogGroupField' {percent} -> percent) (\s@LogGroupField' {} a -> s {percent = a} :: LogGroupField)

instance Data.FromJSON LogGroupField where
  parseJSON =
    Data.withObject
      "LogGroupField"
      ( \x ->
          LogGroupField'
            Prelude.<$> (x Data..:? "name")
            Prelude.<*> (x Data..:? "percent")
      )

instance Prelude.Hashable LogGroupField where
  hashWithSalt _salt LogGroupField' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` percent

instance Prelude.NFData LogGroupField where
  rnf LogGroupField' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf percent
