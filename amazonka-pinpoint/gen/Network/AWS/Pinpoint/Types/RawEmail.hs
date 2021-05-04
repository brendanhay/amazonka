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
-- Module      : Network.AWS.Pinpoint.Types.RawEmail
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.RawEmail where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Specifies the contents of an email message, represented as a raw MIME
-- message.
--
-- /See:/ 'newRawEmail' smart constructor.
data RawEmail = RawEmail'
  { -- | The email message, represented as a raw MIME message. The entire message
    -- must be base64 encoded.
    data' :: Prelude.Maybe Prelude.Base64
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'RawEmail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'data'', 'rawEmail_data' - The email message, represented as a raw MIME message. The entire message
-- must be base64 encoded.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
newRawEmail ::
  RawEmail
newRawEmail = RawEmail' {data' = Prelude.Nothing}

-- | The email message, represented as a raw MIME message. The entire message
-- must be base64 encoded.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
rawEmail_data :: Lens.Lens' RawEmail (Prelude.Maybe Prelude.ByteString)
rawEmail_data = Lens.lens (\RawEmail' {data'} -> data') (\s@RawEmail' {} a -> s {data' = a} :: RawEmail) Prelude.. Lens.mapping Prelude._Base64

instance Prelude.Hashable RawEmail

instance Prelude.NFData RawEmail

instance Prelude.ToJSON RawEmail where
  toJSON RawEmail' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [("Data" Prelude..=) Prelude.<$> data']
      )
