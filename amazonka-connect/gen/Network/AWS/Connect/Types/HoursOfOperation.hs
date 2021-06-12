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
-- Module      : Network.AWS.Connect.Types.HoursOfOperation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.HoursOfOperation where

import Network.AWS.Connect.Types.HoursOfOperationConfig
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Information about of the hours of operation.
--
-- /See:/ 'newHoursOfOperation' smart constructor.
data HoursOfOperation = HoursOfOperation'
  { -- | Configuration information for the hours of operation.
    config :: Core.Maybe [HoursOfOperationConfig],
    -- | The Amazon Resource Name (ARN) for the hours of operation.
    hoursOfOperationArn :: Core.Maybe Core.Text,
    -- | The name for the hours of operation.
    name :: Core.Maybe Core.Text,
    -- | One or more tags.
    tags :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | The description for the hours of operation.
    description :: Core.Maybe Core.Text,
    -- | The time zone for the hours of operation.
    timeZone :: Core.Maybe Core.Text,
    -- | The identifier for the hours of operation.
    hoursOfOperationId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'HoursOfOperation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'config', 'hoursOfOperation_config' - Configuration information for the hours of operation.
--
-- 'hoursOfOperationArn', 'hoursOfOperation_hoursOfOperationArn' - The Amazon Resource Name (ARN) for the hours of operation.
--
-- 'name', 'hoursOfOperation_name' - The name for the hours of operation.
--
-- 'tags', 'hoursOfOperation_tags' - One or more tags.
--
-- 'description', 'hoursOfOperation_description' - The description for the hours of operation.
--
-- 'timeZone', 'hoursOfOperation_timeZone' - The time zone for the hours of operation.
--
-- 'hoursOfOperationId', 'hoursOfOperation_hoursOfOperationId' - The identifier for the hours of operation.
newHoursOfOperation ::
  HoursOfOperation
newHoursOfOperation =
  HoursOfOperation'
    { config = Core.Nothing,
      hoursOfOperationArn = Core.Nothing,
      name = Core.Nothing,
      tags = Core.Nothing,
      description = Core.Nothing,
      timeZone = Core.Nothing,
      hoursOfOperationId = Core.Nothing
    }

-- | Configuration information for the hours of operation.
hoursOfOperation_config :: Lens.Lens' HoursOfOperation (Core.Maybe [HoursOfOperationConfig])
hoursOfOperation_config = Lens.lens (\HoursOfOperation' {config} -> config) (\s@HoursOfOperation' {} a -> s {config = a} :: HoursOfOperation) Core.. Lens.mapping Lens._Coerce

-- | The Amazon Resource Name (ARN) for the hours of operation.
hoursOfOperation_hoursOfOperationArn :: Lens.Lens' HoursOfOperation (Core.Maybe Core.Text)
hoursOfOperation_hoursOfOperationArn = Lens.lens (\HoursOfOperation' {hoursOfOperationArn} -> hoursOfOperationArn) (\s@HoursOfOperation' {} a -> s {hoursOfOperationArn = a} :: HoursOfOperation)

-- | The name for the hours of operation.
hoursOfOperation_name :: Lens.Lens' HoursOfOperation (Core.Maybe Core.Text)
hoursOfOperation_name = Lens.lens (\HoursOfOperation' {name} -> name) (\s@HoursOfOperation' {} a -> s {name = a} :: HoursOfOperation)

-- | One or more tags.
hoursOfOperation_tags :: Lens.Lens' HoursOfOperation (Core.Maybe (Core.HashMap Core.Text Core.Text))
hoursOfOperation_tags = Lens.lens (\HoursOfOperation' {tags} -> tags) (\s@HoursOfOperation' {} a -> s {tags = a} :: HoursOfOperation) Core.. Lens.mapping Lens._Coerce

-- | The description for the hours of operation.
hoursOfOperation_description :: Lens.Lens' HoursOfOperation (Core.Maybe Core.Text)
hoursOfOperation_description = Lens.lens (\HoursOfOperation' {description} -> description) (\s@HoursOfOperation' {} a -> s {description = a} :: HoursOfOperation)

-- | The time zone for the hours of operation.
hoursOfOperation_timeZone :: Lens.Lens' HoursOfOperation (Core.Maybe Core.Text)
hoursOfOperation_timeZone = Lens.lens (\HoursOfOperation' {timeZone} -> timeZone) (\s@HoursOfOperation' {} a -> s {timeZone = a} :: HoursOfOperation)

-- | The identifier for the hours of operation.
hoursOfOperation_hoursOfOperationId :: Lens.Lens' HoursOfOperation (Core.Maybe Core.Text)
hoursOfOperation_hoursOfOperationId = Lens.lens (\HoursOfOperation' {hoursOfOperationId} -> hoursOfOperationId) (\s@HoursOfOperation' {} a -> s {hoursOfOperationId = a} :: HoursOfOperation)

instance Core.FromJSON HoursOfOperation where
  parseJSON =
    Core.withObject
      "HoursOfOperation"
      ( \x ->
          HoursOfOperation'
            Core.<$> (x Core..:? "Config" Core..!= Core.mempty)
            Core.<*> (x Core..:? "HoursOfOperationArn")
            Core.<*> (x Core..:? "Name")
            Core.<*> (x Core..:? "Tags" Core..!= Core.mempty)
            Core.<*> (x Core..:? "Description")
            Core.<*> (x Core..:? "TimeZone")
            Core.<*> (x Core..:? "HoursOfOperationId")
      )

instance Core.Hashable HoursOfOperation

instance Core.NFData HoursOfOperation
