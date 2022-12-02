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
-- Module      : Amazonka.DataBrew.Types.DatetimeOptions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataBrew.Types.DatetimeOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents additional options for correct interpretation of datetime
-- parameters used in the Amazon S3 path of a dataset.
--
-- /See:/ 'newDatetimeOptions' smart constructor.
data DatetimeOptions = DatetimeOptions'
  { -- | Optional value for a non-US locale code, needed for correct
    -- interpretation of some date formats.
    localeCode :: Prelude.Maybe Prelude.Text,
    -- | Optional value for a timezone offset of the datetime parameter value in
    -- the Amazon S3 path. Shouldn\'t be used if Format for this parameter
    -- includes timezone fields. If no offset specified, UTC is assumed.
    timezoneOffset :: Prelude.Maybe Prelude.Text,
    -- | Required option, that defines the datetime format used for a date
    -- parameter in the Amazon S3 path. Should use only supported datetime
    -- specifiers and separation characters, all literal a-z or A-Z characters
    -- should be escaped with single quotes. E.g. \"MM.dd.yyyy-\'at\'-HH:mm\".
    format :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DatetimeOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'localeCode', 'datetimeOptions_localeCode' - Optional value for a non-US locale code, needed for correct
-- interpretation of some date formats.
--
-- 'timezoneOffset', 'datetimeOptions_timezoneOffset' - Optional value for a timezone offset of the datetime parameter value in
-- the Amazon S3 path. Shouldn\'t be used if Format for this parameter
-- includes timezone fields. If no offset specified, UTC is assumed.
--
-- 'format', 'datetimeOptions_format' - Required option, that defines the datetime format used for a date
-- parameter in the Amazon S3 path. Should use only supported datetime
-- specifiers and separation characters, all literal a-z or A-Z characters
-- should be escaped with single quotes. E.g. \"MM.dd.yyyy-\'at\'-HH:mm\".
newDatetimeOptions ::
  -- | 'format'
  Prelude.Text ->
  DatetimeOptions
newDatetimeOptions pFormat_ =
  DatetimeOptions'
    { localeCode = Prelude.Nothing,
      timezoneOffset = Prelude.Nothing,
      format = pFormat_
    }

-- | Optional value for a non-US locale code, needed for correct
-- interpretation of some date formats.
datetimeOptions_localeCode :: Lens.Lens' DatetimeOptions (Prelude.Maybe Prelude.Text)
datetimeOptions_localeCode = Lens.lens (\DatetimeOptions' {localeCode} -> localeCode) (\s@DatetimeOptions' {} a -> s {localeCode = a} :: DatetimeOptions)

-- | Optional value for a timezone offset of the datetime parameter value in
-- the Amazon S3 path. Shouldn\'t be used if Format for this parameter
-- includes timezone fields. If no offset specified, UTC is assumed.
datetimeOptions_timezoneOffset :: Lens.Lens' DatetimeOptions (Prelude.Maybe Prelude.Text)
datetimeOptions_timezoneOffset = Lens.lens (\DatetimeOptions' {timezoneOffset} -> timezoneOffset) (\s@DatetimeOptions' {} a -> s {timezoneOffset = a} :: DatetimeOptions)

-- | Required option, that defines the datetime format used for a date
-- parameter in the Amazon S3 path. Should use only supported datetime
-- specifiers and separation characters, all literal a-z or A-Z characters
-- should be escaped with single quotes. E.g. \"MM.dd.yyyy-\'at\'-HH:mm\".
datetimeOptions_format :: Lens.Lens' DatetimeOptions Prelude.Text
datetimeOptions_format = Lens.lens (\DatetimeOptions' {format} -> format) (\s@DatetimeOptions' {} a -> s {format = a} :: DatetimeOptions)

instance Data.FromJSON DatetimeOptions where
  parseJSON =
    Data.withObject
      "DatetimeOptions"
      ( \x ->
          DatetimeOptions'
            Prelude.<$> (x Data..:? "LocaleCode")
            Prelude.<*> (x Data..:? "TimezoneOffset")
            Prelude.<*> (x Data..: "Format")
      )

instance Prelude.Hashable DatetimeOptions where
  hashWithSalt _salt DatetimeOptions' {..} =
    _salt `Prelude.hashWithSalt` localeCode
      `Prelude.hashWithSalt` timezoneOffset
      `Prelude.hashWithSalt` format

instance Prelude.NFData DatetimeOptions where
  rnf DatetimeOptions' {..} =
    Prelude.rnf localeCode
      `Prelude.seq` Prelude.rnf timezoneOffset
      `Prelude.seq` Prelude.rnf format

instance Data.ToJSON DatetimeOptions where
  toJSON DatetimeOptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("LocaleCode" Data..=) Prelude.<$> localeCode,
            ("TimezoneOffset" Data..=)
              Prelude.<$> timezoneOffset,
            Prelude.Just ("Format" Data..= format)
          ]
      )
