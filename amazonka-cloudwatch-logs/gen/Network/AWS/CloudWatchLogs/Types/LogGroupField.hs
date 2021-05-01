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
-- Module      : Network.AWS.CloudWatchLogs.Types.LogGroupField
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchLogs.Types.LogGroupField where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.FromJSON LogGroupField where
  parseJSON =
    Prelude.withObject
      "LogGroupField"
      ( \x ->
          LogGroupField'
            Prelude.<$> (x Prelude..:? "name")
            Prelude.<*> (x Prelude..:? "percent")
      )

instance Prelude.Hashable LogGroupField

instance Prelude.NFData LogGroupField
