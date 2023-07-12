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
-- Module      : Amazonka.LexV2Models.Types.TextInputSpecification
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.TextInputSpecification where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies the text input specifications.
--
-- /See:/ 'newTextInputSpecification' smart constructor.
data TextInputSpecification = TextInputSpecification'
  { -- | Time for which a bot waits before re-prompting a customer for text
    -- input.
    startTimeoutMs :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TextInputSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'startTimeoutMs', 'textInputSpecification_startTimeoutMs' - Time for which a bot waits before re-prompting a customer for text
-- input.
newTextInputSpecification ::
  -- | 'startTimeoutMs'
  Prelude.Natural ->
  TextInputSpecification
newTextInputSpecification pStartTimeoutMs_ =
  TextInputSpecification'
    { startTimeoutMs =
        pStartTimeoutMs_
    }

-- | Time for which a bot waits before re-prompting a customer for text
-- input.
textInputSpecification_startTimeoutMs :: Lens.Lens' TextInputSpecification Prelude.Natural
textInputSpecification_startTimeoutMs = Lens.lens (\TextInputSpecification' {startTimeoutMs} -> startTimeoutMs) (\s@TextInputSpecification' {} a -> s {startTimeoutMs = a} :: TextInputSpecification)

instance Data.FromJSON TextInputSpecification where
  parseJSON =
    Data.withObject
      "TextInputSpecification"
      ( \x ->
          TextInputSpecification'
            Prelude.<$> (x Data..: "startTimeoutMs")
      )

instance Prelude.Hashable TextInputSpecification where
  hashWithSalt _salt TextInputSpecification' {..} =
    _salt `Prelude.hashWithSalt` startTimeoutMs

instance Prelude.NFData TextInputSpecification where
  rnf TextInputSpecification' {..} =
    Prelude.rnf startTimeoutMs

instance Data.ToJSON TextInputSpecification where
  toJSON TextInputSpecification' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("startTimeoutMs" Data..= startTimeoutMs)
          ]
      )
