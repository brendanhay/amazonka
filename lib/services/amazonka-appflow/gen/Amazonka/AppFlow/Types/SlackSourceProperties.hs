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
-- Module      : Amazonka.AppFlow.Types.SlackSourceProperties
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Types.SlackSourceProperties where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The properties that are applied when Slack is being used as a source.
--
-- /See:/ 'newSlackSourceProperties' smart constructor.
data SlackSourceProperties = SlackSourceProperties'
  { -- | The object specified in the Slack flow source.
    object' :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SlackSourceProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'object'', 'slackSourceProperties_object' - The object specified in the Slack flow source.
newSlackSourceProperties ::
  -- | 'object''
  Prelude.Text ->
  SlackSourceProperties
newSlackSourceProperties pObject_ =
  SlackSourceProperties' {object' = pObject_}

-- | The object specified in the Slack flow source.
slackSourceProperties_object :: Lens.Lens' SlackSourceProperties Prelude.Text
slackSourceProperties_object = Lens.lens (\SlackSourceProperties' {object'} -> object') (\s@SlackSourceProperties' {} a -> s {object' = a} :: SlackSourceProperties)

instance Data.FromJSON SlackSourceProperties where
  parseJSON =
    Data.withObject
      "SlackSourceProperties"
      ( \x ->
          SlackSourceProperties'
            Prelude.<$> (x Data..: "object")
      )

instance Prelude.Hashable SlackSourceProperties where
  hashWithSalt _salt SlackSourceProperties' {..} =
    _salt `Prelude.hashWithSalt` object'

instance Prelude.NFData SlackSourceProperties where
  rnf SlackSourceProperties' {..} = Prelude.rnf object'

instance Data.ToJSON SlackSourceProperties where
  toJSON SlackSourceProperties' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("object" Data..= object')]
      )
