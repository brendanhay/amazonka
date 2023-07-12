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
-- Module      : Amazonka.PinpointEmail.Types.Content
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PinpointEmail.Types.Content where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object that represents the content of the email, and optionally a
-- character set specification.
--
-- /See:/ 'newContent' smart constructor.
data Content = Content'
  { -- | The character set for the content. Because of the constraints of the
    -- SMTP protocol, Amazon Pinpoint uses 7-bit ASCII by default. If the text
    -- includes characters outside of the ASCII range, you have to specify a
    -- character set. For example, you could specify @UTF-8@, @ISO-8859-1@, or
    -- @Shift_JIS@.
    charset :: Prelude.Maybe Prelude.Text,
    -- | The content of the message itself.
    data' :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Content' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'charset', 'content_charset' - The character set for the content. Because of the constraints of the
-- SMTP protocol, Amazon Pinpoint uses 7-bit ASCII by default. If the text
-- includes characters outside of the ASCII range, you have to specify a
-- character set. For example, you could specify @UTF-8@, @ISO-8859-1@, or
-- @Shift_JIS@.
--
-- 'data'', 'content_data' - The content of the message itself.
newContent ::
  -- | 'data''
  Prelude.Text ->
  Content
newContent pData_ =
  Content' {charset = Prelude.Nothing, data' = pData_}

-- | The character set for the content. Because of the constraints of the
-- SMTP protocol, Amazon Pinpoint uses 7-bit ASCII by default. If the text
-- includes characters outside of the ASCII range, you have to specify a
-- character set. For example, you could specify @UTF-8@, @ISO-8859-1@, or
-- @Shift_JIS@.
content_charset :: Lens.Lens' Content (Prelude.Maybe Prelude.Text)
content_charset = Lens.lens (\Content' {charset} -> charset) (\s@Content' {} a -> s {charset = a} :: Content)

-- | The content of the message itself.
content_data :: Lens.Lens' Content Prelude.Text
content_data = Lens.lens (\Content' {data'} -> data') (\s@Content' {} a -> s {data' = a} :: Content)

instance Prelude.Hashable Content where
  hashWithSalt _salt Content' {..} =
    _salt
      `Prelude.hashWithSalt` charset
      `Prelude.hashWithSalt` data'

instance Prelude.NFData Content where
  rnf Content' {..} =
    Prelude.rnf charset `Prelude.seq` Prelude.rnf data'

instance Data.ToJSON Content where
  toJSON Content' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Charset" Data..=) Prelude.<$> charset,
            Prelude.Just ("Data" Data..= data')
          ]
      )
