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
-- Module      : Amazonka.AppFlow.Types.SlackConnectorProfileProperties
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Types.SlackConnectorProfileProperties where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The connector-specific profile properties required when using Slack.
--
-- /See:/ 'newSlackConnectorProfileProperties' smart constructor.
data SlackConnectorProfileProperties = SlackConnectorProfileProperties'
  { -- | The location of the Slack resource.
    instanceUrl :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SlackConnectorProfileProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceUrl', 'slackConnectorProfileProperties_instanceUrl' - The location of the Slack resource.
newSlackConnectorProfileProperties ::
  -- | 'instanceUrl'
  Prelude.Text ->
  SlackConnectorProfileProperties
newSlackConnectorProfileProperties pInstanceUrl_ =
  SlackConnectorProfileProperties'
    { instanceUrl =
        pInstanceUrl_
    }

-- | The location of the Slack resource.
slackConnectorProfileProperties_instanceUrl :: Lens.Lens' SlackConnectorProfileProperties Prelude.Text
slackConnectorProfileProperties_instanceUrl = Lens.lens (\SlackConnectorProfileProperties' {instanceUrl} -> instanceUrl) (\s@SlackConnectorProfileProperties' {} a -> s {instanceUrl = a} :: SlackConnectorProfileProperties)

instance
  Data.FromJSON
    SlackConnectorProfileProperties
  where
  parseJSON =
    Data.withObject
      "SlackConnectorProfileProperties"
      ( \x ->
          SlackConnectorProfileProperties'
            Prelude.<$> (x Data..: "instanceUrl")
      )

instance
  Prelude.Hashable
    SlackConnectorProfileProperties
  where
  hashWithSalt
    _salt
    SlackConnectorProfileProperties' {..} =
      _salt `Prelude.hashWithSalt` instanceUrl

instance
  Prelude.NFData
    SlackConnectorProfileProperties
  where
  rnf SlackConnectorProfileProperties' {..} =
    Prelude.rnf instanceUrl

instance Data.ToJSON SlackConnectorProfileProperties where
  toJSON SlackConnectorProfileProperties' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("instanceUrl" Data..= instanceUrl)]
      )
