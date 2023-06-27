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
-- Module      : Amazonka.Support.Types.SupportedHour
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Support.Types.SupportedHour where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Time range object with @startTime@ and @endTime@ range in RFC 3339
-- format. @\'HH:mm:ss.SSS\'@.
--
-- /See:/ 'newSupportedHour' smart constructor.
data SupportedHour = SupportedHour'
  { -- | End Time. RFC 3339 format @\'HH:mm:ss.SSS\'@.
    endTime :: Prelude.Maybe Prelude.Text,
    -- | Start Time. RFC 3339 format @\'HH:mm:ss.SSS\'@.
    startTime :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SupportedHour' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endTime', 'supportedHour_endTime' - End Time. RFC 3339 format @\'HH:mm:ss.SSS\'@.
--
-- 'startTime', 'supportedHour_startTime' - Start Time. RFC 3339 format @\'HH:mm:ss.SSS\'@.
newSupportedHour ::
  SupportedHour
newSupportedHour =
  SupportedHour'
    { endTime = Prelude.Nothing,
      startTime = Prelude.Nothing
    }

-- | End Time. RFC 3339 format @\'HH:mm:ss.SSS\'@.
supportedHour_endTime :: Lens.Lens' SupportedHour (Prelude.Maybe Prelude.Text)
supportedHour_endTime = Lens.lens (\SupportedHour' {endTime} -> endTime) (\s@SupportedHour' {} a -> s {endTime = a} :: SupportedHour)

-- | Start Time. RFC 3339 format @\'HH:mm:ss.SSS\'@.
supportedHour_startTime :: Lens.Lens' SupportedHour (Prelude.Maybe Prelude.Text)
supportedHour_startTime = Lens.lens (\SupportedHour' {startTime} -> startTime) (\s@SupportedHour' {} a -> s {startTime = a} :: SupportedHour)

instance Data.FromJSON SupportedHour where
  parseJSON =
    Data.withObject
      "SupportedHour"
      ( \x ->
          SupportedHour'
            Prelude.<$> (x Data..:? "endTime")
            Prelude.<*> (x Data..:? "startTime")
      )

instance Prelude.Hashable SupportedHour where
  hashWithSalt _salt SupportedHour' {..} =
    _salt
      `Prelude.hashWithSalt` endTime
      `Prelude.hashWithSalt` startTime

instance Prelude.NFData SupportedHour where
  rnf SupportedHour' {..} =
    Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf startTime
