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
-- Module      : Amazonka.LexV2Models.Types.SSMLMessage
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.SSMLMessage where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Defines a Speech Synthesis Markup Language (SSML) prompt.
--
-- /See:/ 'newSSMLMessage' smart constructor.
data SSMLMessage = SSMLMessage'
  { -- | The SSML text that defines the prompt.
    value :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SSMLMessage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'value', 'sSMLMessage_value' - The SSML text that defines the prompt.
newSSMLMessage ::
  -- | 'value'
  Prelude.Text ->
  SSMLMessage
newSSMLMessage pValue_ =
  SSMLMessage' {value = pValue_}

-- | The SSML text that defines the prompt.
sSMLMessage_value :: Lens.Lens' SSMLMessage Prelude.Text
sSMLMessage_value = Lens.lens (\SSMLMessage' {value} -> value) (\s@SSMLMessage' {} a -> s {value = a} :: SSMLMessage)

instance Data.FromJSON SSMLMessage where
  parseJSON =
    Data.withObject
      "SSMLMessage"
      (\x -> SSMLMessage' Prelude.<$> (x Data..: "value"))

instance Prelude.Hashable SSMLMessage where
  hashWithSalt _salt SSMLMessage' {..} =
    _salt `Prelude.hashWithSalt` value

instance Prelude.NFData SSMLMessage where
  rnf SSMLMessage' {..} = Prelude.rnf value

instance Data.ToJSON SSMLMessage where
  toJSON SSMLMessage' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("value" Data..= value)]
      )
