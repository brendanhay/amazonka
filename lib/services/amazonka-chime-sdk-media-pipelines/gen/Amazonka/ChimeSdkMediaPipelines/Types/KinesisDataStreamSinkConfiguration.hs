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
-- Module      : Amazonka.ChimeSdkMediaPipelines.Types.KinesisDataStreamSinkConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSdkMediaPipelines.Types.KinesisDataStreamSinkConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A structure that contains the configuration settings for a Kinesis Data
-- Stream sink.
--
-- /See:/ 'newKinesisDataStreamSinkConfiguration' smart constructor.
data KinesisDataStreamSinkConfiguration = KinesisDataStreamSinkConfiguration'
  { -- | The ARN of the sink.
    insightsTarget :: Prelude.Maybe (Data.Sensitive Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'KinesisDataStreamSinkConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'insightsTarget', 'kinesisDataStreamSinkConfiguration_insightsTarget' - The ARN of the sink.
newKinesisDataStreamSinkConfiguration ::
  KinesisDataStreamSinkConfiguration
newKinesisDataStreamSinkConfiguration =
  KinesisDataStreamSinkConfiguration'
    { insightsTarget =
        Prelude.Nothing
    }

-- | The ARN of the sink.
kinesisDataStreamSinkConfiguration_insightsTarget :: Lens.Lens' KinesisDataStreamSinkConfiguration (Prelude.Maybe Prelude.Text)
kinesisDataStreamSinkConfiguration_insightsTarget = Lens.lens (\KinesisDataStreamSinkConfiguration' {insightsTarget} -> insightsTarget) (\s@KinesisDataStreamSinkConfiguration' {} a -> s {insightsTarget = a} :: KinesisDataStreamSinkConfiguration) Prelude.. Lens.mapping Data._Sensitive

instance
  Data.FromJSON
    KinesisDataStreamSinkConfiguration
  where
  parseJSON =
    Data.withObject
      "KinesisDataStreamSinkConfiguration"
      ( \x ->
          KinesisDataStreamSinkConfiguration'
            Prelude.<$> (x Data..:? "InsightsTarget")
      )

instance
  Prelude.Hashable
    KinesisDataStreamSinkConfiguration
  where
  hashWithSalt
    _salt
    KinesisDataStreamSinkConfiguration' {..} =
      _salt `Prelude.hashWithSalt` insightsTarget

instance
  Prelude.NFData
    KinesisDataStreamSinkConfiguration
  where
  rnf KinesisDataStreamSinkConfiguration' {..} =
    Prelude.rnf insightsTarget

instance
  Data.ToJSON
    KinesisDataStreamSinkConfiguration
  where
  toJSON KinesisDataStreamSinkConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("InsightsTarget" Data..=)
              Prelude.<$> insightsTarget
          ]
      )
