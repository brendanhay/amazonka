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
-- Module      : Amazonka.Pinpoint.Types.SimpleEmailPart
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.SimpleEmailPart where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies the subject or body of an email message, represented as
-- textual email data and the applicable character set.
--
-- /See:/ 'newSimpleEmailPart' smart constructor.
data SimpleEmailPart = SimpleEmailPart'
  { -- | The applicable character set for the message content.
    charset :: Prelude.Maybe Prelude.Text,
    -- | The textual data of the message content.
    data' :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SimpleEmailPart' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'charset', 'simpleEmailPart_charset' - The applicable character set for the message content.
--
-- 'data'', 'simpleEmailPart_data' - The textual data of the message content.
newSimpleEmailPart ::
  SimpleEmailPart
newSimpleEmailPart =
  SimpleEmailPart'
    { charset = Prelude.Nothing,
      data' = Prelude.Nothing
    }

-- | The applicable character set for the message content.
simpleEmailPart_charset :: Lens.Lens' SimpleEmailPart (Prelude.Maybe Prelude.Text)
simpleEmailPart_charset = Lens.lens (\SimpleEmailPart' {charset} -> charset) (\s@SimpleEmailPart' {} a -> s {charset = a} :: SimpleEmailPart)

-- | The textual data of the message content.
simpleEmailPart_data :: Lens.Lens' SimpleEmailPart (Prelude.Maybe Prelude.Text)
simpleEmailPart_data = Lens.lens (\SimpleEmailPart' {data'} -> data') (\s@SimpleEmailPart' {} a -> s {data' = a} :: SimpleEmailPart)

instance Prelude.Hashable SimpleEmailPart where
  hashWithSalt _salt SimpleEmailPart' {..} =
    _salt
      `Prelude.hashWithSalt` charset
      `Prelude.hashWithSalt` data'

instance Prelude.NFData SimpleEmailPart where
  rnf SimpleEmailPart' {..} =
    Prelude.rnf charset `Prelude.seq` Prelude.rnf data'

instance Data.ToJSON SimpleEmailPart where
  toJSON SimpleEmailPart' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Charset" Data..=) Prelude.<$> charset,
            ("Data" Data..=) Prelude.<$> data'
          ]
      )
