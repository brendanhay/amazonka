{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes the configuration manager.
--
-- /See:/ 'newStackConfigurationManager' smart constructor.
data StackConfigurationManager = StackConfigurationManager'
  { -- | The Chef version. This parameter must be set to 12, 11.10, or 11.4 for
    -- Linux stacks, and to 12.2 for Windows stacks. The default value for
    -- Linux stacks is 11.4.
    version :: Prelude.Maybe Prelude.Text,
    -- | The name. This parameter must be set to \"Chef\".
    name :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { version =
        Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | The Chef version. This parameter must be set to 12, 11.10, or 11.4 for
-- Linux stacks, and to 12.2 for Windows stacks. The default value for
-- Linux stacks is 11.4.
stackConfigurationManager_version :: Lens.Lens' StackConfigurationManager (Prelude.Maybe Prelude.Text)
stackConfigurationManager_version = Lens.lens (\StackConfigurationManager' {version} -> version) (\s@StackConfigurationManager' {} a -> s {version = a} :: StackConfigurationManager)

-- | The name. This parameter must be set to \"Chef\".
stackConfigurationManager_name :: Lens.Lens' StackConfigurationManager (Prelude.Maybe Prelude.Text)
stackConfigurationManager_name = Lens.lens (\StackConfigurationManager' {name} -> name) (\s@StackConfigurationManager' {} a -> s {name = a} :: StackConfigurationManager)

instance Prelude.FromJSON StackConfigurationManager where
  parseJSON =
    Prelude.withObject
      "StackConfigurationManager"
      ( \x ->
          StackConfigurationManager'
            Prelude.<$> (x Prelude..:? "Version")
            Prelude.<*> (x Prelude..:? "Name")
      )

instance Prelude.Hashable StackConfigurationManager

instance Prelude.NFData StackConfigurationManager

instance Prelude.ToJSON StackConfigurationManager where
  toJSON StackConfigurationManager' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("Version" Prelude..=) Prelude.<$> version,
            ("Name" Prelude..=) Prelude.<$> name
          ]
      )
