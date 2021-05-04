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
-- Module      : Network.AWS.Pinpoint.Types.SimpleEmailPart
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.SimpleEmailPart where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Specifies the subject or body of an email message, represented as
-- textual email data and the applicable character set.
--
-- /See:/ 'newSimpleEmailPart' smart constructor.
data SimpleEmailPart = SimpleEmailPart'
  { -- | The textual data of the message content.
    data' :: Prelude.Maybe Prelude.Text,
    -- | The applicable character set for the message content.
    charset :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'SimpleEmailPart' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'data'', 'simpleEmailPart_data' - The textual data of the message content.
--
-- 'charset', 'simpleEmailPart_charset' - The applicable character set for the message content.
newSimpleEmailPart ::
  SimpleEmailPart
newSimpleEmailPart =
  SimpleEmailPart'
    { data' = Prelude.Nothing,
      charset = Prelude.Nothing
    }

-- | The textual data of the message content.
simpleEmailPart_data :: Lens.Lens' SimpleEmailPart (Prelude.Maybe Prelude.Text)
simpleEmailPart_data = Lens.lens (\SimpleEmailPart' {data'} -> data') (\s@SimpleEmailPart' {} a -> s {data' = a} :: SimpleEmailPart)

-- | The applicable character set for the message content.
simpleEmailPart_charset :: Lens.Lens' SimpleEmailPart (Prelude.Maybe Prelude.Text)
simpleEmailPart_charset = Lens.lens (\SimpleEmailPart' {charset} -> charset) (\s@SimpleEmailPart' {} a -> s {charset = a} :: SimpleEmailPart)

instance Prelude.Hashable SimpleEmailPart

instance Prelude.NFData SimpleEmailPart

instance Prelude.ToJSON SimpleEmailPart where
  toJSON SimpleEmailPart' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("Data" Prelude..=) Prelude.<$> data',
            ("Charset" Prelude..=) Prelude.<$> charset
          ]
      )
