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
-- Module      : Amazonka.ChimeSdkMediaPipelines.Types.SnsTopicSinkConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSdkMediaPipelines.Types.SnsTopicSinkConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The configuration settings for the SNS topic sink.
--
-- /See:/ 'newSnsTopicSinkConfiguration' smart constructor.
data SnsTopicSinkConfiguration = SnsTopicSinkConfiguration'
  { -- | The ARN of the SNS sink.
    insightsTarget :: Prelude.Maybe (Data.Sensitive Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SnsTopicSinkConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'insightsTarget', 'snsTopicSinkConfiguration_insightsTarget' - The ARN of the SNS sink.
newSnsTopicSinkConfiguration ::
  SnsTopicSinkConfiguration
newSnsTopicSinkConfiguration =
  SnsTopicSinkConfiguration'
    { insightsTarget =
        Prelude.Nothing
    }

-- | The ARN of the SNS sink.
snsTopicSinkConfiguration_insightsTarget :: Lens.Lens' SnsTopicSinkConfiguration (Prelude.Maybe Prelude.Text)
snsTopicSinkConfiguration_insightsTarget = Lens.lens (\SnsTopicSinkConfiguration' {insightsTarget} -> insightsTarget) (\s@SnsTopicSinkConfiguration' {} a -> s {insightsTarget = a} :: SnsTopicSinkConfiguration) Prelude.. Lens.mapping Data._Sensitive

instance Data.FromJSON SnsTopicSinkConfiguration where
  parseJSON =
    Data.withObject
      "SnsTopicSinkConfiguration"
      ( \x ->
          SnsTopicSinkConfiguration'
            Prelude.<$> (x Data..:? "InsightsTarget")
      )

instance Prelude.Hashable SnsTopicSinkConfiguration where
  hashWithSalt _salt SnsTopicSinkConfiguration' {..} =
    _salt `Prelude.hashWithSalt` insightsTarget

instance Prelude.NFData SnsTopicSinkConfiguration where
  rnf SnsTopicSinkConfiguration' {..} =
    Prelude.rnf insightsTarget

instance Data.ToJSON SnsTopicSinkConfiguration where
  toJSON SnsTopicSinkConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("InsightsTarget" Data..=)
              Prelude.<$> insightsTarget
          ]
      )
