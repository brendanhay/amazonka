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
-- Module      : Amazonka.SmsVoice.Types.CallInstructionsMessageType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SmsVoice.Types.CallInstructionsMessageType where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object that defines a message that contains text formatted using
-- Amazon Pinpoint Voice Instructions markup.
--
-- /See:/ 'newCallInstructionsMessageType' smart constructor.
data CallInstructionsMessageType = CallInstructionsMessageType'
  { -- | The language to use when delivering the message. For a complete list of
    -- supported languages, see the Amazon Polly Developer Guide.
    text :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CallInstructionsMessageType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'text', 'callInstructionsMessageType_text' - The language to use when delivering the message. For a complete list of
-- supported languages, see the Amazon Polly Developer Guide.
newCallInstructionsMessageType ::
  CallInstructionsMessageType
newCallInstructionsMessageType =
  CallInstructionsMessageType'
    { text =
        Prelude.Nothing
    }

-- | The language to use when delivering the message. For a complete list of
-- supported languages, see the Amazon Polly Developer Guide.
callInstructionsMessageType_text :: Lens.Lens' CallInstructionsMessageType (Prelude.Maybe Prelude.Text)
callInstructionsMessageType_text = Lens.lens (\CallInstructionsMessageType' {text} -> text) (\s@CallInstructionsMessageType' {} a -> s {text = a} :: CallInstructionsMessageType)

instance Prelude.Hashable CallInstructionsMessageType where
  hashWithSalt _salt CallInstructionsMessageType' {..} =
    _salt `Prelude.hashWithSalt` text

instance Prelude.NFData CallInstructionsMessageType where
  rnf CallInstructionsMessageType' {..} =
    Prelude.rnf text

instance Data.ToJSON CallInstructionsMessageType where
  toJSON CallInstructionsMessageType' {..} =
    Data.object
      ( Prelude.catMaybes
          [("Text" Data..=) Prelude.<$> text]
      )
