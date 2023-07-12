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
-- Module      : Amazonka.LexV2Models.Types.BotAliasLocaleSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.BotAliasLocaleSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types.CodeHookSpecification
import qualified Amazonka.Prelude as Prelude

-- | Specifies settings that are unique to a locale. For example, you can use
-- different Lambda function depending on the bot\'s locale.
--
-- /See:/ 'newBotAliasLocaleSettings' smart constructor.
data BotAliasLocaleSettings = BotAliasLocaleSettings'
  { -- | Specifies the Lambda function that should be used in the locale.
    codeHookSpecification :: Prelude.Maybe CodeHookSpecification,
    -- | Determines whether the locale is enabled for the bot. If the value is
    -- @false@, the locale isn\'t available for use.
    enabled :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BotAliasLocaleSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'codeHookSpecification', 'botAliasLocaleSettings_codeHookSpecification' - Specifies the Lambda function that should be used in the locale.
--
-- 'enabled', 'botAliasLocaleSettings_enabled' - Determines whether the locale is enabled for the bot. If the value is
-- @false@, the locale isn\'t available for use.
newBotAliasLocaleSettings ::
  -- | 'enabled'
  Prelude.Bool ->
  BotAliasLocaleSettings
newBotAliasLocaleSettings pEnabled_ =
  BotAliasLocaleSettings'
    { codeHookSpecification =
        Prelude.Nothing,
      enabled = pEnabled_
    }

-- | Specifies the Lambda function that should be used in the locale.
botAliasLocaleSettings_codeHookSpecification :: Lens.Lens' BotAliasLocaleSettings (Prelude.Maybe CodeHookSpecification)
botAliasLocaleSettings_codeHookSpecification = Lens.lens (\BotAliasLocaleSettings' {codeHookSpecification} -> codeHookSpecification) (\s@BotAliasLocaleSettings' {} a -> s {codeHookSpecification = a} :: BotAliasLocaleSettings)

-- | Determines whether the locale is enabled for the bot. If the value is
-- @false@, the locale isn\'t available for use.
botAliasLocaleSettings_enabled :: Lens.Lens' BotAliasLocaleSettings Prelude.Bool
botAliasLocaleSettings_enabled = Lens.lens (\BotAliasLocaleSettings' {enabled} -> enabled) (\s@BotAliasLocaleSettings' {} a -> s {enabled = a} :: BotAliasLocaleSettings)

instance Data.FromJSON BotAliasLocaleSettings where
  parseJSON =
    Data.withObject
      "BotAliasLocaleSettings"
      ( \x ->
          BotAliasLocaleSettings'
            Prelude.<$> (x Data..:? "codeHookSpecification")
            Prelude.<*> (x Data..: "enabled")
      )

instance Prelude.Hashable BotAliasLocaleSettings where
  hashWithSalt _salt BotAliasLocaleSettings' {..} =
    _salt
      `Prelude.hashWithSalt` codeHookSpecification
      `Prelude.hashWithSalt` enabled

instance Prelude.NFData BotAliasLocaleSettings where
  rnf BotAliasLocaleSettings' {..} =
    Prelude.rnf codeHookSpecification
      `Prelude.seq` Prelude.rnf enabled

instance Data.ToJSON BotAliasLocaleSettings where
  toJSON BotAliasLocaleSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("codeHookSpecification" Data..=)
              Prelude.<$> codeHookSpecification,
            Prelude.Just ("enabled" Data..= enabled)
          ]
      )
