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
-- Module      : Network.AWS.Kendra.Types.DocumentAttributeValue
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Kendra.Types.DocumentAttributeValue where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The value of a custom document attribute. You can only provide one value
-- for a custom attribute.
--
-- /See:/ 'newDocumentAttributeValue' smart constructor.
data DocumentAttributeValue = DocumentAttributeValue'
  { -- | A string, such as \"department\".
    stringValue :: Prelude.Maybe Prelude.Text,
    -- | A long integer value.
    longValue :: Prelude.Maybe Prelude.Integer,
    -- | A list of strings.
    stringListValue :: Prelude.Maybe [Prelude.Text],
    -- | A date expressed as an ISO 8601 string.
    --
    -- It is important for the time zone to be included in the ISO 8601
    -- date-time format. For example, 20120325T123010+01:00 is the ISO 8601
    -- date-time format for March 25th 2012 at 12:30PM (plus 10 seconds) in
    -- Central European Time.
    dateValue :: Prelude.Maybe Core.POSIX
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
-- 'stringValue', 'documentAttributeValue_stringValue' - A string, such as \"department\".
--
-- 'longValue', 'documentAttributeValue_longValue' - A long integer value.
--
-- 'stringListValue', 'documentAttributeValue_stringListValue' - A list of strings.
--
-- 'dateValue', 'documentAttributeValue_dateValue' - A date expressed as an ISO 8601 string.
--
-- It is important for the time zone to be included in the ISO 8601
-- date-time format. For example, 20120325T123010+01:00 is the ISO 8601
-- date-time format for March 25th 2012 at 12:30PM (plus 10 seconds) in
-- Central European Time.
newDocumentAttributeValue ::
  DocumentAttributeValue
newDocumentAttributeValue =
  DocumentAttributeValue'
    { stringValue =
        Prelude.Nothing,
      longValue = Prelude.Nothing,
      stringListValue = Prelude.Nothing,
      dateValue = Prelude.Nothing
    }

-- | A string, such as \"department\".
documentAttributeValue_stringValue :: Lens.Lens' DocumentAttributeValue (Prelude.Maybe Prelude.Text)
documentAttributeValue_stringValue = Lens.lens (\DocumentAttributeValue' {stringValue} -> stringValue) (\s@DocumentAttributeValue' {} a -> s {stringValue = a} :: DocumentAttributeValue)

-- | A long integer value.
documentAttributeValue_longValue :: Lens.Lens' DocumentAttributeValue (Prelude.Maybe Prelude.Integer)
documentAttributeValue_longValue = Lens.lens (\DocumentAttributeValue' {longValue} -> longValue) (\s@DocumentAttributeValue' {} a -> s {longValue = a} :: DocumentAttributeValue)

-- | A list of strings.
documentAttributeValue_stringListValue :: Lens.Lens' DocumentAttributeValue (Prelude.Maybe [Prelude.Text])
documentAttributeValue_stringListValue = Lens.lens (\DocumentAttributeValue' {stringListValue} -> stringListValue) (\s@DocumentAttributeValue' {} a -> s {stringListValue = a} :: DocumentAttributeValue) Prelude.. Lens.mapping Lens.coerced

-- | A date expressed as an ISO 8601 string.
--
-- It is important for the time zone to be included in the ISO 8601
-- date-time format. For example, 20120325T123010+01:00 is the ISO 8601
-- date-time format for March 25th 2012 at 12:30PM (plus 10 seconds) in
-- Central European Time.
documentAttributeValue_dateValue :: Lens.Lens' DocumentAttributeValue (Prelude.Maybe Prelude.UTCTime)
documentAttributeValue_dateValue = Lens.lens (\DocumentAttributeValue' {dateValue} -> dateValue) (\s@DocumentAttributeValue' {} a -> s {dateValue = a} :: DocumentAttributeValue) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON DocumentAttributeValue where
  parseJSON =
    Core.withObject
      "DocumentAttributeValue"
      ( \x ->
          DocumentAttributeValue'
            Prelude.<$> (x Core..:? "StringValue")
            Prelude.<*> (x Core..:? "LongValue")
            Prelude.<*> ( x Core..:? "StringListValue"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "DateValue")
      )

instance Prelude.Hashable DocumentAttributeValue

instance Prelude.NFData DocumentAttributeValue

instance Core.ToJSON DocumentAttributeValue where
  toJSON DocumentAttributeValue' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("StringValue" Core..=) Prelude.<$> stringValue,
            ("LongValue" Core..=) Prelude.<$> longValue,
            ("StringListValue" Core..=)
              Prelude.<$> stringListValue,
            ("DateValue" Core..=) Prelude.<$> dateValue
          ]
      )
