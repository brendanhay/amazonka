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
-- Module      : Amazonka.GroundStation.Types.DecodeConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GroundStation.Types.DecodeConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Information about the decode @Config@.
--
-- /See:/ 'newDecodeConfig' smart constructor.
data DecodeConfig = DecodeConfig'
  { -- | Unvalidated JSON of a decode @Config@.
    unvalidatedJSON :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DecodeConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'unvalidatedJSON', 'decodeConfig_unvalidatedJSON' - Unvalidated JSON of a decode @Config@.
newDecodeConfig ::
  -- | 'unvalidatedJSON'
  Prelude.Text ->
  DecodeConfig
newDecodeConfig pUnvalidatedJSON_ =
  DecodeConfig' {unvalidatedJSON = pUnvalidatedJSON_}

-- | Unvalidated JSON of a decode @Config@.
decodeConfig_unvalidatedJSON :: Lens.Lens' DecodeConfig Prelude.Text
decodeConfig_unvalidatedJSON = Lens.lens (\DecodeConfig' {unvalidatedJSON} -> unvalidatedJSON) (\s@DecodeConfig' {} a -> s {unvalidatedJSON = a} :: DecodeConfig)

instance Core.FromJSON DecodeConfig where
  parseJSON =
    Core.withObject
      "DecodeConfig"
      ( \x ->
          DecodeConfig'
            Prelude.<$> (x Core..: "unvalidatedJSON")
      )

instance Prelude.Hashable DecodeConfig where
  hashWithSalt _salt DecodeConfig' {..} =
    _salt `Prelude.hashWithSalt` unvalidatedJSON

instance Prelude.NFData DecodeConfig where
  rnf DecodeConfig' {..} = Prelude.rnf unvalidatedJSON

instance Core.ToJSON DecodeConfig where
  toJSON DecodeConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("unvalidatedJSON" Core..= unvalidatedJSON)
          ]
      )
