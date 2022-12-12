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
-- Module      : Amazonka.Kendra.Types.DocumentAttributeValue
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.DocumentAttributeValue where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The value of a document attribute. You can only provide one value for a
-- document attribute.
--
-- /See:/ 'newDocumentAttributeValue' smart constructor.
data DocumentAttributeValue = DocumentAttributeValue'
  { -- | A date expressed as an ISO 8601 string.
    --
    -- It is important for the time zone to be included in the ISO 8601
    -- date-time format. For example, 2012-03-25T12:30:10+01:00 is the ISO 8601
    -- date-time format for March 25th 2012 at 12:30PM (plus 10 seconds) in
    -- Central European Time.
    dateValue :: Prelude.Maybe Data.POSIX,
    -- | A long integer value.
    longValue :: Prelude.Maybe Prelude.Integer,
    -- | A list of strings. The default maximum length or number of strings is
    -- 10.
    stringListValue :: Prelude.Maybe [Prelude.Text],
    -- | A string, such as \"department\".
    stringValue :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DocumentAttributeValue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dateValue', 'documentAttributeValue_dateValue' - A date expressed as an ISO 8601 string.
--
-- It is important for the time zone to be included in the ISO 8601
-- date-time format. For example, 2012-03-25T12:30:10+01:00 is the ISO 8601
-- date-time format for March 25th 2012 at 12:30PM (plus 10 seconds) in
-- Central European Time.
--
-- 'longValue', 'documentAttributeValue_longValue' - A long integer value.
--
-- 'stringListValue', 'documentAttributeValue_stringListValue' - A list of strings. The default maximum length or number of strings is
-- 10.
--
-- 'stringValue', 'documentAttributeValue_stringValue' - A string, such as \"department\".
newDocumentAttributeValue ::
  DocumentAttributeValue
newDocumentAttributeValue =
  DocumentAttributeValue'
    { dateValue =
        Prelude.Nothing,
      longValue = Prelude.Nothing,
      stringListValue = Prelude.Nothing,
      stringValue = Prelude.Nothing
    }

-- | A date expressed as an ISO 8601 string.
--
-- It is important for the time zone to be included in the ISO 8601
-- date-time format. For example, 2012-03-25T12:30:10+01:00 is the ISO 8601
-- date-time format for March 25th 2012 at 12:30PM (plus 10 seconds) in
-- Central European Time.
documentAttributeValue_dateValue :: Lens.Lens' DocumentAttributeValue (Prelude.Maybe Prelude.UTCTime)
documentAttributeValue_dateValue = Lens.lens (\DocumentAttributeValue' {dateValue} -> dateValue) (\s@DocumentAttributeValue' {} a -> s {dateValue = a} :: DocumentAttributeValue) Prelude.. Lens.mapping Data._Time

-- | A long integer value.
documentAttributeValue_longValue :: Lens.Lens' DocumentAttributeValue (Prelude.Maybe Prelude.Integer)
documentAttributeValue_longValue = Lens.lens (\DocumentAttributeValue' {longValue} -> longValue) (\s@DocumentAttributeValue' {} a -> s {longValue = a} :: DocumentAttributeValue)

-- | A list of strings. The default maximum length or number of strings is
-- 10.
documentAttributeValue_stringListValue :: Lens.Lens' DocumentAttributeValue (Prelude.Maybe [Prelude.Text])
documentAttributeValue_stringListValue = Lens.lens (\DocumentAttributeValue' {stringListValue} -> stringListValue) (\s@DocumentAttributeValue' {} a -> s {stringListValue = a} :: DocumentAttributeValue) Prelude.. Lens.mapping Lens.coerced

-- | A string, such as \"department\".
documentAttributeValue_stringValue :: Lens.Lens' DocumentAttributeValue (Prelude.Maybe Prelude.Text)
documentAttributeValue_stringValue = Lens.lens (\DocumentAttributeValue' {stringValue} -> stringValue) (\s@DocumentAttributeValue' {} a -> s {stringValue = a} :: DocumentAttributeValue)

instance Data.FromJSON DocumentAttributeValue where
  parseJSON =
    Data.withObject
      "DocumentAttributeValue"
      ( \x ->
          DocumentAttributeValue'
            Prelude.<$> (x Data..:? "DateValue")
            Prelude.<*> (x Data..:? "LongValue")
            Prelude.<*> ( x Data..:? "StringListValue"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "StringValue")
      )

instance Prelude.Hashable DocumentAttributeValue where
  hashWithSalt _salt DocumentAttributeValue' {..} =
    _salt `Prelude.hashWithSalt` dateValue
      `Prelude.hashWithSalt` longValue
      `Prelude.hashWithSalt` stringListValue
      `Prelude.hashWithSalt` stringValue

instance Prelude.NFData DocumentAttributeValue where
  rnf DocumentAttributeValue' {..} =
    Prelude.rnf dateValue
      `Prelude.seq` Prelude.rnf longValue
      `Prelude.seq` Prelude.rnf stringListValue
      `Prelude.seq` Prelude.rnf stringValue

instance Data.ToJSON DocumentAttributeValue where
  toJSON DocumentAttributeValue' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DateValue" Data..=) Prelude.<$> dateValue,
            ("LongValue" Data..=) Prelude.<$> longValue,
            ("StringListValue" Data..=)
              Prelude.<$> stringListValue,
            ("StringValue" Data..=) Prelude.<$> stringValue
          ]
      )
