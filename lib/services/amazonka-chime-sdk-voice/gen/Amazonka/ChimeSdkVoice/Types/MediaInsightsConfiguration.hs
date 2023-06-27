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
-- Module      : Amazonka.ChimeSdkVoice.Types.MediaInsightsConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSdkVoice.Types.MediaInsightsConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The configuration for a call analytics task.
--
-- /See:/ 'newMediaInsightsConfiguration' smart constructor.
data MediaInsightsConfiguration = MediaInsightsConfiguration'
  { -- | The configuration\'s ARN.
    configurationArn :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | Denotes the configration as enabled or disabled.
    disabled :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MediaInsightsConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configurationArn', 'mediaInsightsConfiguration_configurationArn' - The configuration\'s ARN.
--
-- 'disabled', 'mediaInsightsConfiguration_disabled' - Denotes the configration as enabled or disabled.
newMediaInsightsConfiguration ::
  MediaInsightsConfiguration
newMediaInsightsConfiguration =
  MediaInsightsConfiguration'
    { configurationArn =
        Prelude.Nothing,
      disabled = Prelude.Nothing
    }

-- | The configuration\'s ARN.
mediaInsightsConfiguration_configurationArn :: Lens.Lens' MediaInsightsConfiguration (Prelude.Maybe Prelude.Text)
mediaInsightsConfiguration_configurationArn = Lens.lens (\MediaInsightsConfiguration' {configurationArn} -> configurationArn) (\s@MediaInsightsConfiguration' {} a -> s {configurationArn = a} :: MediaInsightsConfiguration) Prelude.. Lens.mapping Data._Sensitive

-- | Denotes the configration as enabled or disabled.
mediaInsightsConfiguration_disabled :: Lens.Lens' MediaInsightsConfiguration (Prelude.Maybe Prelude.Bool)
mediaInsightsConfiguration_disabled = Lens.lens (\MediaInsightsConfiguration' {disabled} -> disabled) (\s@MediaInsightsConfiguration' {} a -> s {disabled = a} :: MediaInsightsConfiguration)

instance Data.FromJSON MediaInsightsConfiguration where
  parseJSON =
    Data.withObject
      "MediaInsightsConfiguration"
      ( \x ->
          MediaInsightsConfiguration'
            Prelude.<$> (x Data..:? "ConfigurationArn")
            Prelude.<*> (x Data..:? "Disabled")
      )

instance Prelude.Hashable MediaInsightsConfiguration where
  hashWithSalt _salt MediaInsightsConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` configurationArn
      `Prelude.hashWithSalt` disabled

instance Prelude.NFData MediaInsightsConfiguration where
  rnf MediaInsightsConfiguration' {..} =
    Prelude.rnf configurationArn
      `Prelude.seq` Prelude.rnf disabled

instance Data.ToJSON MediaInsightsConfiguration where
  toJSON MediaInsightsConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ConfigurationArn" Data..=)
              Prelude.<$> configurationArn,
            ("Disabled" Data..=) Prelude.<$> disabled
          ]
      )
