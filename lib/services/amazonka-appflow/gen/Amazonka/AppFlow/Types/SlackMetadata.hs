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
-- Module      : Amazonka.AppFlow.Types.SlackMetadata
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Types.SlackMetadata where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The connector metadata specific to Slack.
--
-- /See:/ 'newSlackMetadata' smart constructor.
data SlackMetadata = SlackMetadata'
  { -- | The desired authorization scope for the Slack account.
    oAuthScopes :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SlackMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'oAuthScopes', 'slackMetadata_oAuthScopes' - The desired authorization scope for the Slack account.
newSlackMetadata ::
  SlackMetadata
newSlackMetadata =
  SlackMetadata' {oAuthScopes = Prelude.Nothing}

-- | The desired authorization scope for the Slack account.
slackMetadata_oAuthScopes :: Lens.Lens' SlackMetadata (Prelude.Maybe [Prelude.Text])
slackMetadata_oAuthScopes = Lens.lens (\SlackMetadata' {oAuthScopes} -> oAuthScopes) (\s@SlackMetadata' {} a -> s {oAuthScopes = a} :: SlackMetadata) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON SlackMetadata where
  parseJSON =
    Data.withObject
      "SlackMetadata"
      ( \x ->
          SlackMetadata'
            Prelude.<$> (x Data..:? "oAuthScopes" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable SlackMetadata where
  hashWithSalt _salt SlackMetadata' {..} =
    _salt `Prelude.hashWithSalt` oAuthScopes

instance Prelude.NFData SlackMetadata where
  rnf SlackMetadata' {..} = Prelude.rnf oAuthScopes
