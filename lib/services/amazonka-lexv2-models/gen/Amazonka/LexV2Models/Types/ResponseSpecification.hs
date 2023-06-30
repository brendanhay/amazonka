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
-- Module      : Amazonka.LexV2Models.Types.ResponseSpecification
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.ResponseSpecification where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types.MessageGroup
import qualified Amazonka.Prelude as Prelude

-- | Specifies a list of message groups that Amazon Lex uses to respond the
-- user input.
--
-- /See:/ 'newResponseSpecification' smart constructor.
data ResponseSpecification = ResponseSpecification'
  { -- | Indicates whether the user can interrupt a speech response from Amazon
    -- Lex.
    allowInterrupt :: Prelude.Maybe Prelude.Bool,
    -- | A collection of responses that Amazon Lex can send to the user. Amazon
    -- Lex chooses the actual response to send at runtime.
    messageGroups :: Prelude.NonEmpty MessageGroup
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResponseSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'allowInterrupt', 'responseSpecification_allowInterrupt' - Indicates whether the user can interrupt a speech response from Amazon
-- Lex.
--
-- 'messageGroups', 'responseSpecification_messageGroups' - A collection of responses that Amazon Lex can send to the user. Amazon
-- Lex chooses the actual response to send at runtime.
newResponseSpecification ::
  -- | 'messageGroups'
  Prelude.NonEmpty MessageGroup ->
  ResponseSpecification
newResponseSpecification pMessageGroups_ =
  ResponseSpecification'
    { allowInterrupt =
        Prelude.Nothing,
      messageGroups = Lens.coerced Lens.# pMessageGroups_
    }

-- | Indicates whether the user can interrupt a speech response from Amazon
-- Lex.
responseSpecification_allowInterrupt :: Lens.Lens' ResponseSpecification (Prelude.Maybe Prelude.Bool)
responseSpecification_allowInterrupt = Lens.lens (\ResponseSpecification' {allowInterrupt} -> allowInterrupt) (\s@ResponseSpecification' {} a -> s {allowInterrupt = a} :: ResponseSpecification)

-- | A collection of responses that Amazon Lex can send to the user. Amazon
-- Lex chooses the actual response to send at runtime.
responseSpecification_messageGroups :: Lens.Lens' ResponseSpecification (Prelude.NonEmpty MessageGroup)
responseSpecification_messageGroups = Lens.lens (\ResponseSpecification' {messageGroups} -> messageGroups) (\s@ResponseSpecification' {} a -> s {messageGroups = a} :: ResponseSpecification) Prelude.. Lens.coerced

instance Data.FromJSON ResponseSpecification where
  parseJSON =
    Data.withObject
      "ResponseSpecification"
      ( \x ->
          ResponseSpecification'
            Prelude.<$> (x Data..:? "allowInterrupt")
            Prelude.<*> (x Data..: "messageGroups")
      )

instance Prelude.Hashable ResponseSpecification where
  hashWithSalt _salt ResponseSpecification' {..} =
    _salt
      `Prelude.hashWithSalt` allowInterrupt
      `Prelude.hashWithSalt` messageGroups

instance Prelude.NFData ResponseSpecification where
  rnf ResponseSpecification' {..} =
    Prelude.rnf allowInterrupt
      `Prelude.seq` Prelude.rnf messageGroups

instance Data.ToJSON ResponseSpecification where
  toJSON ResponseSpecification' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("allowInterrupt" Data..=)
              Prelude.<$> allowInterrupt,
            Prelude.Just
              ("messageGroups" Data..= messageGroups)
          ]
      )
