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
-- Module      : Network.AWS.CodePipeline.Types.ActionConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.ActionConfiguration where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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

instance Core.FromJSON ActionConfiguration where
  parseJSON =
    Core.withObject
      "ActionConfiguration"
      ( \x ->
          ActionConfiguration'
            Prelude.<$> (x Core..:? "configuration" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable ActionConfiguration

instance Prelude.NFData ActionConfiguration
