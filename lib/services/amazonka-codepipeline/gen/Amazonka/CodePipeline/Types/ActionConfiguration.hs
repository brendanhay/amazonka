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
-- Module      : Amazonka.CodePipeline.Types.ActionConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodePipeline.Types.ActionConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents information about an action configuration.
--
-- /See:/ 'newActionConfiguration' smart constructor.
data ActionConfiguration = ActionConfiguration'
  { -- | The configuration data for the action.
    configuration :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ActionConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configuration', 'actionConfiguration_configuration' - The configuration data for the action.
newActionConfiguration ::
  ActionConfiguration
newActionConfiguration =
  ActionConfiguration'
    { configuration =
        Prelude.Nothing
    }

-- | The configuration data for the action.
actionConfiguration_configuration :: Lens.Lens' ActionConfiguration (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
actionConfiguration_configuration = Lens.lens (\ActionConfiguration' {configuration} -> configuration) (\s@ActionConfiguration' {} a -> s {configuration = a} :: ActionConfiguration) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON ActionConfiguration where
  parseJSON =
    Data.withObject
      "ActionConfiguration"
      ( \x ->
          ActionConfiguration'
            Prelude.<$> (x Data..:? "configuration" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable ActionConfiguration where
  hashWithSalt _salt ActionConfiguration' {..} =
    _salt `Prelude.hashWithSalt` configuration

instance Prelude.NFData ActionConfiguration where
  rnf ActionConfiguration' {..} =
    Prelude.rnf configuration
