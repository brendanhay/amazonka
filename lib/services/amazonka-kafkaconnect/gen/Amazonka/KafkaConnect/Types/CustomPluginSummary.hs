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
-- Module      : Amazonka.KafkaConnect.Types.CustomPluginSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KafkaConnect.Types.CustomPluginSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KafkaConnect.Types.CustomPluginRevisionSummary
import Amazonka.KafkaConnect.Types.CustomPluginState
import qualified Amazonka.Prelude as Prelude

-- | A summary of the custom plugin.
--
-- /See:/ 'newCustomPluginSummary' smart constructor.
data CustomPluginSummary = CustomPluginSummary'
  { -- | The time that the custom plugin was created.
    creationTime :: Prelude.Maybe Data.ISO8601,
    -- | The Amazon Resource Name (ARN) of the custom plugin.
    customPluginArn :: Prelude.Maybe Prelude.Text,
    -- | The state of the custom plugin.
    customPluginState :: Prelude.Maybe CustomPluginState,
    -- | A description of the custom plugin.
    description :: Prelude.Maybe Prelude.Text,
    -- | The latest revision of the custom plugin.
    latestRevision :: Prelude.Maybe CustomPluginRevisionSummary,
    -- | The name of the custom plugin.
    name :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CustomPluginSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'customPluginSummary_creationTime' - The time that the custom plugin was created.
--
-- 'customPluginArn', 'customPluginSummary_customPluginArn' - The Amazon Resource Name (ARN) of the custom plugin.
--
-- 'customPluginState', 'customPluginSummary_customPluginState' - The state of the custom plugin.
--
-- 'description', 'customPluginSummary_description' - A description of the custom plugin.
--
-- 'latestRevision', 'customPluginSummary_latestRevision' - The latest revision of the custom plugin.
--
-- 'name', 'customPluginSummary_name' - The name of the custom plugin.
newCustomPluginSummary ::
  CustomPluginSummary
newCustomPluginSummary =
  CustomPluginSummary'
    { creationTime =
        Prelude.Nothing,
      customPluginArn = Prelude.Nothing,
      customPluginState = Prelude.Nothing,
      description = Prelude.Nothing,
      latestRevision = Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | The time that the custom plugin was created.
customPluginSummary_creationTime :: Lens.Lens' CustomPluginSummary (Prelude.Maybe Prelude.UTCTime)
customPluginSummary_creationTime = Lens.lens (\CustomPluginSummary' {creationTime} -> creationTime) (\s@CustomPluginSummary' {} a -> s {creationTime = a} :: CustomPluginSummary) Prelude.. Lens.mapping Data._Time

-- | The Amazon Resource Name (ARN) of the custom plugin.
customPluginSummary_customPluginArn :: Lens.Lens' CustomPluginSummary (Prelude.Maybe Prelude.Text)
customPluginSummary_customPluginArn = Lens.lens (\CustomPluginSummary' {customPluginArn} -> customPluginArn) (\s@CustomPluginSummary' {} a -> s {customPluginArn = a} :: CustomPluginSummary)

-- | The state of the custom plugin.
customPluginSummary_customPluginState :: Lens.Lens' CustomPluginSummary (Prelude.Maybe CustomPluginState)
customPluginSummary_customPluginState = Lens.lens (\CustomPluginSummary' {customPluginState} -> customPluginState) (\s@CustomPluginSummary' {} a -> s {customPluginState = a} :: CustomPluginSummary)

-- | A description of the custom plugin.
customPluginSummary_description :: Lens.Lens' CustomPluginSummary (Prelude.Maybe Prelude.Text)
customPluginSummary_description = Lens.lens (\CustomPluginSummary' {description} -> description) (\s@CustomPluginSummary' {} a -> s {description = a} :: CustomPluginSummary)

-- | The latest revision of the custom plugin.
customPluginSummary_latestRevision :: Lens.Lens' CustomPluginSummary (Prelude.Maybe CustomPluginRevisionSummary)
customPluginSummary_latestRevision = Lens.lens (\CustomPluginSummary' {latestRevision} -> latestRevision) (\s@CustomPluginSummary' {} a -> s {latestRevision = a} :: CustomPluginSummary)

-- | The name of the custom plugin.
customPluginSummary_name :: Lens.Lens' CustomPluginSummary (Prelude.Maybe Prelude.Text)
customPluginSummary_name = Lens.lens (\CustomPluginSummary' {name} -> name) (\s@CustomPluginSummary' {} a -> s {name = a} :: CustomPluginSummary)

instance Data.FromJSON CustomPluginSummary where
  parseJSON =
    Data.withObject
      "CustomPluginSummary"
      ( \x ->
          CustomPluginSummary'
            Prelude.<$> (x Data..:? "creationTime")
            Prelude.<*> (x Data..:? "customPluginArn")
            Prelude.<*> (x Data..:? "customPluginState")
            Prelude.<*> (x Data..:? "description")
            Prelude.<*> (x Data..:? "latestRevision")
            Prelude.<*> (x Data..:? "name")
      )

instance Prelude.Hashable CustomPluginSummary where
  hashWithSalt _salt CustomPluginSummary' {..} =
    _salt
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` customPluginArn
      `Prelude.hashWithSalt` customPluginState
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` latestRevision
      `Prelude.hashWithSalt` name

instance Prelude.NFData CustomPluginSummary where
  rnf CustomPluginSummary' {..} =
    Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf customPluginArn
      `Prelude.seq` Prelude.rnf customPluginState
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf latestRevision
      `Prelude.seq` Prelude.rnf name
