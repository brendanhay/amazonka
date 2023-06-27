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
-- Module      : Amazonka.ChimeSDKIdentity.Types.Configuration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSDKIdentity.Types.Configuration where

import Amazonka.ChimeSDKIdentity.Types.LexConfiguration
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A structure that contains configuration data.
--
-- /See:/ 'newConfiguration' smart constructor.
data Configuration = Configuration'
  { -- | The configuration for an Amazon Lex V2 bot.
    lex' :: LexConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Configuration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lex'', 'configuration_lex' - The configuration for an Amazon Lex V2 bot.
newConfiguration ::
  -- | 'lex''
  LexConfiguration ->
  Configuration
newConfiguration pLex_ = Configuration' {lex' = pLex_}

-- | The configuration for an Amazon Lex V2 bot.
configuration_lex :: Lens.Lens' Configuration LexConfiguration
configuration_lex = Lens.lens (\Configuration' {lex'} -> lex') (\s@Configuration' {} a -> s {lex' = a} :: Configuration)

instance Data.FromJSON Configuration where
  parseJSON =
    Data.withObject
      "Configuration"
      (\x -> Configuration' Prelude.<$> (x Data..: "Lex"))

instance Prelude.Hashable Configuration where
  hashWithSalt _salt Configuration' {..} =
    _salt `Prelude.hashWithSalt` lex'

instance Prelude.NFData Configuration where
  rnf Configuration' {..} = Prelude.rnf lex'

instance Data.ToJSON Configuration where
  toJSON Configuration' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Lex" Data..= lex')]
      )
