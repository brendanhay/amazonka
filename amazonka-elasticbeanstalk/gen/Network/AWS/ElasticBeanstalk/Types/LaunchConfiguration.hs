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
-- Module      : Network.AWS.ElasticBeanstalk.Types.LaunchConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.LaunchConfiguration where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes an Auto Scaling launch configuration.
--
-- /See:/ 'newLaunchConfiguration' smart constructor.
data LaunchConfiguration = LaunchConfiguration'
  { -- | The name of the launch configuration.
    name :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'LaunchConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'launchConfiguration_name' - The name of the launch configuration.
newLaunchConfiguration ::
  LaunchConfiguration
newLaunchConfiguration =
  LaunchConfiguration' {name = Prelude.Nothing}

-- | The name of the launch configuration.
launchConfiguration_name :: Lens.Lens' LaunchConfiguration (Prelude.Maybe Prelude.Text)
launchConfiguration_name = Lens.lens (\LaunchConfiguration' {name} -> name) (\s@LaunchConfiguration' {} a -> s {name = a} :: LaunchConfiguration)

instance Prelude.FromXML LaunchConfiguration where
  parseXML x =
    LaunchConfiguration'
      Prelude.<$> (x Prelude..@? "Name")

instance Prelude.Hashable LaunchConfiguration

instance Prelude.NFData LaunchConfiguration
