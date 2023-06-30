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
-- Module      : Amazonka.ElasticBeanstalk.Types.PlatformFramework
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElasticBeanstalk.Types.PlatformFramework where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A framework supported by the platform.
--
-- /See:/ 'newPlatformFramework' smart constructor.
data PlatformFramework = PlatformFramework'
  { -- | The name of the framework.
    name :: Prelude.Maybe Prelude.Text,
    -- | The version of the framework.
    version :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PlatformFramework' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'platformFramework_name' - The name of the framework.
--
-- 'version', 'platformFramework_version' - The version of the framework.
newPlatformFramework ::
  PlatformFramework
newPlatformFramework =
  PlatformFramework'
    { name = Prelude.Nothing,
      version = Prelude.Nothing
    }

-- | The name of the framework.
platformFramework_name :: Lens.Lens' PlatformFramework (Prelude.Maybe Prelude.Text)
platformFramework_name = Lens.lens (\PlatformFramework' {name} -> name) (\s@PlatformFramework' {} a -> s {name = a} :: PlatformFramework)

-- | The version of the framework.
platformFramework_version :: Lens.Lens' PlatformFramework (Prelude.Maybe Prelude.Text)
platformFramework_version = Lens.lens (\PlatformFramework' {version} -> version) (\s@PlatformFramework' {} a -> s {version = a} :: PlatformFramework)

instance Data.FromXML PlatformFramework where
  parseXML x =
    PlatformFramework'
      Prelude.<$> (x Data..@? "Name")
      Prelude.<*> (x Data..@? "Version")

instance Prelude.Hashable PlatformFramework where
  hashWithSalt _salt PlatformFramework' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` version

instance Prelude.NFData PlatformFramework where
  rnf PlatformFramework' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf version
