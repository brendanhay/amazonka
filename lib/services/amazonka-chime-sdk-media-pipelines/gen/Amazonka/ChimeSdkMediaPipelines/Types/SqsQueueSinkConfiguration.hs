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
-- Module      : Amazonka.ChimeSdkMediaPipelines.Types.SqsQueueSinkConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSdkMediaPipelines.Types.SqsQueueSinkConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The configuration settings for the SQS sink.
--
-- /See:/ 'newSqsQueueSinkConfiguration' smart constructor.
data SqsQueueSinkConfiguration = SqsQueueSinkConfiguration'
  { -- | The ARN of the SQS sink.
    insightsTarget :: Prelude.Maybe (Data.Sensitive Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SqsQueueSinkConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'insightsTarget', 'sqsQueueSinkConfiguration_insightsTarget' - The ARN of the SQS sink.
newSqsQueueSinkConfiguration ::
  SqsQueueSinkConfiguration
newSqsQueueSinkConfiguration =
  SqsQueueSinkConfiguration'
    { insightsTarget =
        Prelude.Nothing
    }

-- | The ARN of the SQS sink.
sqsQueueSinkConfiguration_insightsTarget :: Lens.Lens' SqsQueueSinkConfiguration (Prelude.Maybe Prelude.Text)
sqsQueueSinkConfiguration_insightsTarget = Lens.lens (\SqsQueueSinkConfiguration' {insightsTarget} -> insightsTarget) (\s@SqsQueueSinkConfiguration' {} a -> s {insightsTarget = a} :: SqsQueueSinkConfiguration) Prelude.. Lens.mapping Data._Sensitive

instance Data.FromJSON SqsQueueSinkConfiguration where
  parseJSON =
    Data.withObject
      "SqsQueueSinkConfiguration"
      ( \x ->
          SqsQueueSinkConfiguration'
            Prelude.<$> (x Data..:? "InsightsTarget")
      )

instance Prelude.Hashable SqsQueueSinkConfiguration where
  hashWithSalt _salt SqsQueueSinkConfiguration' {..} =
    _salt `Prelude.hashWithSalt` insightsTarget

instance Prelude.NFData SqsQueueSinkConfiguration where
  rnf SqsQueueSinkConfiguration' {..} =
    Prelude.rnf insightsTarget

instance Data.ToJSON SqsQueueSinkConfiguration where
  toJSON SqsQueueSinkConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("InsightsTarget" Data..=)
              Prelude.<$> insightsTarget
          ]
      )
