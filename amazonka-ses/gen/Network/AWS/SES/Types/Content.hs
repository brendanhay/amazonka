{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.SES.Types.Content
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SES.Types.Content where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Represents textual data, plus an optional character set specification.
--
-- By default, the text must be 7-bit ASCII, due to the constraints of the
-- SMTP protocol. If the text must contain any other characters, then you
-- must also specify a character set. Examples include UTF-8, ISO-8859-1,
-- and Shift_JIS.
--
-- /See:/ 'newContent' smart constructor.
data Content = Content'
  { -- | The character set of the content.
    charset :: Prelude.Maybe Prelude.Text,
    -- | The textual data of the content.
    data' :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Content' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'charset', 'content_charset' - The character set of the content.
--
-- 'data'', 'content_data' - The textual data of the content.
newContent ::
  -- | 'data''
  Prelude.Text ->
  Content
newContent pData_ =
  Content' {charset = Prelude.Nothing, data' = pData_}

-- | The character set of the content.
content_charset :: Lens.Lens' Content (Prelude.Maybe Prelude.Text)
content_charset = Lens.lens (\Content' {charset} -> charset) (\s@Content' {} a -> s {charset = a} :: Content)

-- | The textual data of the content.
content_data :: Lens.Lens' Content Prelude.Text
content_data = Lens.lens (\Content' {data'} -> data') (\s@Content' {} a -> s {data' = a} :: Content)

instance Prelude.Hashable Content

instance Prelude.NFData Content

instance Prelude.ToQuery Content where
  toQuery Content' {..} =
    Prelude.mconcat
      [ "Charset" Prelude.=: charset,
        "Data" Prelude.=: data'
      ]
