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
-- Module      : Amazonka.MediaLive.Types.ImmediateModeScheduleActionStartSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.ImmediateModeScheduleActionStartSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Settings to configure an action so that it occurs as soon as possible.
--
-- /See:/ 'newImmediateModeScheduleActionStartSettings' smart constructor.
data ImmediateModeScheduleActionStartSettings = ImmediateModeScheduleActionStartSettings'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImmediateModeScheduleActionStartSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newImmediateModeScheduleActionStartSettings ::
  ImmediateModeScheduleActionStartSettings
newImmediateModeScheduleActionStartSettings =
  ImmediateModeScheduleActionStartSettings'

instance
  Data.FromJSON
    ImmediateModeScheduleActionStartSettings
  where
  parseJSON =
    Data.withObject
      "ImmediateModeScheduleActionStartSettings"
      ( \x ->
          Prelude.pure
            ImmediateModeScheduleActionStartSettings'
      )

instance
  Prelude.Hashable
    ImmediateModeScheduleActionStartSettings
  where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance
  Prelude.NFData
    ImmediateModeScheduleActionStartSettings
  where
  rnf _ = ()

instance
  Data.ToJSON
    ImmediateModeScheduleActionStartSettings
  where
  toJSON = Prelude.const (Data.Object Prelude.mempty)
