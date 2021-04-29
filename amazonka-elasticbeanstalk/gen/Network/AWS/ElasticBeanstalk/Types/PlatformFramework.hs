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
-- Module      : Network.AWS.ElasticBeanstalk.Types.PlatformFramework
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.PlatformFramework where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A framework supported by the platform.
--
-- /See:/ 'newPlatformFramework' smart constructor.
data PlatformFramework = PlatformFramework'
  { -- | The version of the framework.
    version :: Prelude.Maybe Prelude.Text,
    -- | The name of the framework.
    name :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'PlatformFramework' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'version', 'platformFramework_version' - The version of the framework.
--
-- 'name', 'platformFramework_name' - The name of the framework.
newPlatformFramework ::
  PlatformFramework
newPlatformFramework =
  PlatformFramework'
    { version = Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | The version of the framework.
platformFramework_version :: Lens.Lens' PlatformFramework (Prelude.Maybe Prelude.Text)
platformFramework_version = Lens.lens (\PlatformFramework' {version} -> version) (\s@PlatformFramework' {} a -> s {version = a} :: PlatformFramework)

-- | The name of the framework.
platformFramework_name :: Lens.Lens' PlatformFramework (Prelude.Maybe Prelude.Text)
platformFramework_name = Lens.lens (\PlatformFramework' {name} -> name) (\s@PlatformFramework' {} a -> s {name = a} :: PlatformFramework)

instance Prelude.FromXML PlatformFramework where
  parseXML x =
    PlatformFramework'
      Prelude.<$> (x Prelude..@? "Version")
      Prelude.<*> (x Prelude..@? "Name")

instance Prelude.Hashable PlatformFramework

instance Prelude.NFData PlatformFramework
