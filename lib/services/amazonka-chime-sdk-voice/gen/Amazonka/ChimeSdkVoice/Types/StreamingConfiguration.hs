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
-- Module      : Amazonka.ChimeSdkVoice.Types.StreamingConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSdkVoice.Types.StreamingConfiguration where

import Amazonka.ChimeSdkVoice.Types.StreamingNotificationTarget
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | /See:/ 'newStreamingConfiguration' smart constructor.
data StreamingConfiguration = StreamingConfiguration'
  { streamingNotificationTargets :: Prelude.Maybe (Prelude.NonEmpty StreamingNotificationTarget),
    dataRetentionInHours :: Prelude.Natural,
    disabled :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StreamingConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'streamingNotificationTargets', 'streamingConfiguration_streamingNotificationTargets' - Undocumented member.
--
-- 'dataRetentionInHours', 'streamingConfiguration_dataRetentionInHours' - Undocumented member.
--
-- 'disabled', 'streamingConfiguration_disabled' - Undocumented member.
newStreamingConfiguration ::
  -- | 'dataRetentionInHours'
  Prelude.Natural ->
  -- | 'disabled'
  Prelude.Bool ->
  StreamingConfiguration
newStreamingConfiguration
  pDataRetentionInHours_
  pDisabled_ =
    StreamingConfiguration'
      { streamingNotificationTargets =
          Prelude.Nothing,
        dataRetentionInHours = pDataRetentionInHours_,
        disabled = pDisabled_
      }

-- | Undocumented member.
streamingConfiguration_streamingNotificationTargets :: Lens.Lens' StreamingConfiguration (Prelude.Maybe (Prelude.NonEmpty StreamingNotificationTarget))
streamingConfiguration_streamingNotificationTargets = Lens.lens (\StreamingConfiguration' {streamingNotificationTargets} -> streamingNotificationTargets) (\s@StreamingConfiguration' {} a -> s {streamingNotificationTargets = a} :: StreamingConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
streamingConfiguration_dataRetentionInHours :: Lens.Lens' StreamingConfiguration Prelude.Natural
streamingConfiguration_dataRetentionInHours = Lens.lens (\StreamingConfiguration' {dataRetentionInHours} -> dataRetentionInHours) (\s@StreamingConfiguration' {} a -> s {dataRetentionInHours = a} :: StreamingConfiguration)

-- | Undocumented member.
streamingConfiguration_disabled :: Lens.Lens' StreamingConfiguration Prelude.Bool
streamingConfiguration_disabled = Lens.lens (\StreamingConfiguration' {disabled} -> disabled) (\s@StreamingConfiguration' {} a -> s {disabled = a} :: StreamingConfiguration)

instance Data.FromJSON StreamingConfiguration where
  parseJSON =
    Data.withObject
      "StreamingConfiguration"
      ( \x ->
          StreamingConfiguration'
            Prelude.<$> (x Data..:? "StreamingNotificationTargets")
            Prelude.<*> (x Data..: "DataRetentionInHours")
            Prelude.<*> (x Data..: "Disabled")
      )

instance Prelude.Hashable StreamingConfiguration where
  hashWithSalt _salt StreamingConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` streamingNotificationTargets
      `Prelude.hashWithSalt` dataRetentionInHours
      `Prelude.hashWithSalt` disabled

instance Prelude.NFData StreamingConfiguration where
  rnf StreamingConfiguration' {..} =
    Prelude.rnf streamingNotificationTargets
      `Prelude.seq` Prelude.rnf dataRetentionInHours
      `Prelude.seq` Prelude.rnf disabled

instance Data.ToJSON StreamingConfiguration where
  toJSON StreamingConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("StreamingNotificationTargets" Data..=)
              Prelude.<$> streamingNotificationTargets,
            Prelude.Just
              ( "DataRetentionInHours"
                  Data..= dataRetentionInHours
              ),
            Prelude.Just ("Disabled" Data..= disabled)
          ]
      )
