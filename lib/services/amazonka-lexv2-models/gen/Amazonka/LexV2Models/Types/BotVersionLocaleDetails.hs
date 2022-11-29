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
-- Module      : Amazonka.LexV2Models.Types.BotVersionLocaleDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.BotVersionLocaleDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The version of a bot used for a bot locale.
--
-- /See:/ 'newBotVersionLocaleDetails' smart constructor.
data BotVersionLocaleDetails = BotVersionLocaleDetails'
  { -- | The version of a bot used for a bot locale.
    sourceBotVersion :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BotVersionLocaleDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sourceBotVersion', 'botVersionLocaleDetails_sourceBotVersion' - The version of a bot used for a bot locale.
newBotVersionLocaleDetails ::
  -- | 'sourceBotVersion'
  Prelude.Text ->
  BotVersionLocaleDetails
newBotVersionLocaleDetails pSourceBotVersion_ =
  BotVersionLocaleDetails'
    { sourceBotVersion =
        pSourceBotVersion_
    }

-- | The version of a bot used for a bot locale.
botVersionLocaleDetails_sourceBotVersion :: Lens.Lens' BotVersionLocaleDetails Prelude.Text
botVersionLocaleDetails_sourceBotVersion = Lens.lens (\BotVersionLocaleDetails' {sourceBotVersion} -> sourceBotVersion) (\s@BotVersionLocaleDetails' {} a -> s {sourceBotVersion = a} :: BotVersionLocaleDetails)

instance Core.FromJSON BotVersionLocaleDetails where
  parseJSON =
    Core.withObject
      "BotVersionLocaleDetails"
      ( \x ->
          BotVersionLocaleDetails'
            Prelude.<$> (x Core..: "sourceBotVersion")
      )

instance Prelude.Hashable BotVersionLocaleDetails where
  hashWithSalt _salt BotVersionLocaleDetails' {..} =
    _salt `Prelude.hashWithSalt` sourceBotVersion

instance Prelude.NFData BotVersionLocaleDetails where
  rnf BotVersionLocaleDetails' {..} =
    Prelude.rnf sourceBotVersion

instance Core.ToJSON BotVersionLocaleDetails where
  toJSON BotVersionLocaleDetails' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("sourceBotVersion" Core..= sourceBotVersion)
          ]
      )
