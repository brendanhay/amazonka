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
-- Module      : Amazonka.MediaTailor.Types.ClipRange
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaTailor.Types.ClipRange where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Clip range configuration for the VOD source associated with the program.
--
-- /See:/ 'newClipRange' smart constructor.
data ClipRange = ClipRange'
  { -- | The end offset of the clip range, in milliseconds, starting from the
    -- beginning of the VOD source associated with the program.
    endOffsetMillis :: Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ClipRange' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endOffsetMillis', 'clipRange_endOffsetMillis' - The end offset of the clip range, in milliseconds, starting from the
-- beginning of the VOD source associated with the program.
newClipRange ::
  -- | 'endOffsetMillis'
  Prelude.Integer ->
  ClipRange
newClipRange pEndOffsetMillis_ =
  ClipRange' {endOffsetMillis = pEndOffsetMillis_}

-- | The end offset of the clip range, in milliseconds, starting from the
-- beginning of the VOD source associated with the program.
clipRange_endOffsetMillis :: Lens.Lens' ClipRange Prelude.Integer
clipRange_endOffsetMillis = Lens.lens (\ClipRange' {endOffsetMillis} -> endOffsetMillis) (\s@ClipRange' {} a -> s {endOffsetMillis = a} :: ClipRange)

instance Data.FromJSON ClipRange where
  parseJSON =
    Data.withObject
      "ClipRange"
      ( \x ->
          ClipRange' Prelude.<$> (x Data..: "EndOffsetMillis")
      )

instance Prelude.Hashable ClipRange where
  hashWithSalt _salt ClipRange' {..} =
    _salt `Prelude.hashWithSalt` endOffsetMillis

instance Prelude.NFData ClipRange where
  rnf ClipRange' {..} = Prelude.rnf endOffsetMillis

instance Data.ToJSON ClipRange where
  toJSON ClipRange' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("EndOffsetMillis" Data..= endOffsetMillis)
          ]
      )
