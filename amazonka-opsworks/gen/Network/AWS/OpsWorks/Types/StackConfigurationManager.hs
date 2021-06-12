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
-- Module      : Network.AWS.OpsWorks.Types.StackConfigurationManager
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorks.Types.StackConfigurationManager where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Describes the configuration manager.
--
-- /See:/ 'newStackConfigurationManager' smart constructor.
data StackConfigurationManager = StackConfigurationManager'
  { -- | The Chef version. This parameter must be set to 12, 11.10, or 11.4 for
    -- Linux stacks, and to 12.2 for Windows stacks. The default value for
    -- Linux stacks is 11.4.
    version :: Core.Maybe Core.Text,
    -- | The name. This parameter must be set to \"Chef\".
    name :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StackConfigurationManager' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'version', 'stackConfigurationManager_version' - The Chef version. This parameter must be set to 12, 11.10, or 11.4 for
-- Linux stacks, and to 12.2 for Windows stacks. The default value for
-- Linux stacks is 11.4.
--
-- 'name', 'stackConfigurationManager_name' - The name. This parameter must be set to \"Chef\".
newStackConfigurationManager ::
  StackConfigurationManager
newStackConfigurationManager =
  StackConfigurationManager'
    { version = Core.Nothing,
      name = Core.Nothing
    }

-- | The Chef version. This parameter must be set to 12, 11.10, or 11.4 for
-- Linux stacks, and to 12.2 for Windows stacks. The default value for
-- Linux stacks is 11.4.
stackConfigurationManager_version :: Lens.Lens' StackConfigurationManager (Core.Maybe Core.Text)
stackConfigurationManager_version = Lens.lens (\StackConfigurationManager' {version} -> version) (\s@StackConfigurationManager' {} a -> s {version = a} :: StackConfigurationManager)

-- | The name. This parameter must be set to \"Chef\".
stackConfigurationManager_name :: Lens.Lens' StackConfigurationManager (Core.Maybe Core.Text)
stackConfigurationManager_name = Lens.lens (\StackConfigurationManager' {name} -> name) (\s@StackConfigurationManager' {} a -> s {name = a} :: StackConfigurationManager)

instance Core.FromJSON StackConfigurationManager where
  parseJSON =
    Core.withObject
      "StackConfigurationManager"
      ( \x ->
          StackConfigurationManager'
            Core.<$> (x Core..:? "Version") Core.<*> (x Core..:? "Name")
      )

instance Core.Hashable StackConfigurationManager

instance Core.NFData StackConfigurationManager

instance Core.ToJSON StackConfigurationManager where
  toJSON StackConfigurationManager' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Version" Core..=) Core.<$> version,
            ("Name" Core..=) Core.<$> name
          ]
      )
