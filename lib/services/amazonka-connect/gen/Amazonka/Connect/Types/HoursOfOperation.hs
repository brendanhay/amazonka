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
-- Module      : Amazonka.Connect.Types.HoursOfOperation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.HoursOfOperation where

import Amazonka.Connect.Types.HoursOfOperationConfig
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about of the hours of operation.
--
-- /See:/ 'newHoursOfOperation' smart constructor.
data HoursOfOperation = HoursOfOperation'
  { -- | Configuration information for the hours of operation.
    config :: Prelude.Maybe [HoursOfOperationConfig],
    -- | The description for the hours of operation.
    description :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) for the hours of operation.
    hoursOfOperationArn :: Prelude.Maybe Prelude.Text,
    -- | The identifier for the hours of operation.
    hoursOfOperationId :: Prelude.Maybe Prelude.Text,
    -- | The name for the hours of operation.
    name :: Prelude.Maybe Prelude.Text,
    -- | The tags used to organize, track, or control access for this resource.
    -- For example, { \"tags\": {\"key1\":\"value1\", \"key2\":\"value2\"} }.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The time zone for the hours of operation.
    timeZone :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'description', 'hoursOfOperation_description' - The description for the hours of operation.
--
-- 'hoursOfOperationArn', 'hoursOfOperation_hoursOfOperationArn' - The Amazon Resource Name (ARN) for the hours of operation.
--
-- 'hoursOfOperationId', 'hoursOfOperation_hoursOfOperationId' - The identifier for the hours of operation.
--
-- 'name', 'hoursOfOperation_name' - The name for the hours of operation.
--
-- 'tags', 'hoursOfOperation_tags' - The tags used to organize, track, or control access for this resource.
-- For example, { \"tags\": {\"key1\":\"value1\", \"key2\":\"value2\"} }.
--
-- 'timeZone', 'hoursOfOperation_timeZone' - The time zone for the hours of operation.
newHoursOfOperation ::
  HoursOfOperation
newHoursOfOperation =
  HoursOfOperation'
    { config = Prelude.Nothing,
      description = Prelude.Nothing,
      hoursOfOperationArn = Prelude.Nothing,
      hoursOfOperationId = Prelude.Nothing,
      name = Prelude.Nothing,
      tags = Prelude.Nothing,
      timeZone = Prelude.Nothing
    }

-- | Configuration information for the hours of operation.
hoursOfOperation_config :: Lens.Lens' HoursOfOperation (Prelude.Maybe [HoursOfOperationConfig])
hoursOfOperation_config = Lens.lens (\HoursOfOperation' {config} -> config) (\s@HoursOfOperation' {} a -> s {config = a} :: HoursOfOperation) Prelude.. Lens.mapping Lens.coerced

-- | The description for the hours of operation.
hoursOfOperation_description :: Lens.Lens' HoursOfOperation (Prelude.Maybe Prelude.Text)
hoursOfOperation_description = Lens.lens (\HoursOfOperation' {description} -> description) (\s@HoursOfOperation' {} a -> s {description = a} :: HoursOfOperation)

-- | The Amazon Resource Name (ARN) for the hours of operation.
hoursOfOperation_hoursOfOperationArn :: Lens.Lens' HoursOfOperation (Prelude.Maybe Prelude.Text)
hoursOfOperation_hoursOfOperationArn = Lens.lens (\HoursOfOperation' {hoursOfOperationArn} -> hoursOfOperationArn) (\s@HoursOfOperation' {} a -> s {hoursOfOperationArn = a} :: HoursOfOperation)

-- | The identifier for the hours of operation.
hoursOfOperation_hoursOfOperationId :: Lens.Lens' HoursOfOperation (Prelude.Maybe Prelude.Text)
hoursOfOperation_hoursOfOperationId = Lens.lens (\HoursOfOperation' {hoursOfOperationId} -> hoursOfOperationId) (\s@HoursOfOperation' {} a -> s {hoursOfOperationId = a} :: HoursOfOperation)

-- | The name for the hours of operation.
hoursOfOperation_name :: Lens.Lens' HoursOfOperation (Prelude.Maybe Prelude.Text)
hoursOfOperation_name = Lens.lens (\HoursOfOperation' {name} -> name) (\s@HoursOfOperation' {} a -> s {name = a} :: HoursOfOperation)

-- | The tags used to organize, track, or control access for this resource.
-- For example, { \"tags\": {\"key1\":\"value1\", \"key2\":\"value2\"} }.
hoursOfOperation_tags :: Lens.Lens' HoursOfOperation (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
hoursOfOperation_tags = Lens.lens (\HoursOfOperation' {tags} -> tags) (\s@HoursOfOperation' {} a -> s {tags = a} :: HoursOfOperation) Prelude.. Lens.mapping Lens.coerced

-- | The time zone for the hours of operation.
hoursOfOperation_timeZone :: Lens.Lens' HoursOfOperation (Prelude.Maybe Prelude.Text)
hoursOfOperation_timeZone = Lens.lens (\HoursOfOperation' {timeZone} -> timeZone) (\s@HoursOfOperation' {} a -> s {timeZone = a} :: HoursOfOperation)

instance Data.FromJSON HoursOfOperation where
  parseJSON =
    Data.withObject
      "HoursOfOperation"
      ( \x ->
          HoursOfOperation'
            Prelude.<$> (x Data..:? "Config" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "HoursOfOperationArn")
            Prelude.<*> (x Data..:? "HoursOfOperationId")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "Tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "TimeZone")
      )

instance Prelude.Hashable HoursOfOperation where
  hashWithSalt _salt HoursOfOperation' {..} =
    _salt
      `Prelude.hashWithSalt` config
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` hoursOfOperationArn
      `Prelude.hashWithSalt` hoursOfOperationId
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` timeZone

instance Prelude.NFData HoursOfOperation where
  rnf HoursOfOperation' {..} =
    Prelude.rnf config `Prelude.seq`
      Prelude.rnf description `Prelude.seq`
        Prelude.rnf hoursOfOperationArn `Prelude.seq`
          Prelude.rnf hoursOfOperationId `Prelude.seq`
            Prelude.rnf name `Prelude.seq`
              Prelude.rnf tags `Prelude.seq`
                Prelude.rnf timeZone
