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
-- Module      : Network.AWS.EFS.Types.LifecycleConfigurationDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EFS.Types.LifecycleConfigurationDescription where

import qualified Network.AWS.Core as Core
import Network.AWS.EFS.Types.LifecyclePolicy
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | /See:/ 'newLifecycleConfigurationDescription' smart constructor.
data LifecycleConfigurationDescription = LifecycleConfigurationDescription'
  { -- | An array of lifecycle management policies. Currently, EFS supports a
    -- maximum of one policy per file system.
    lifecyclePolicies :: Prelude.Maybe [LifecyclePolicy]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LifecycleConfigurationDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lifecyclePolicies', 'lifecycleConfigurationDescription_lifecyclePolicies' - An array of lifecycle management policies. Currently, EFS supports a
-- maximum of one policy per file system.
newLifecycleConfigurationDescription ::
  LifecycleConfigurationDescription
newLifecycleConfigurationDescription =
  LifecycleConfigurationDescription'
    { lifecyclePolicies =
        Prelude.Nothing
    }

-- | An array of lifecycle management policies. Currently, EFS supports a
-- maximum of one policy per file system.
lifecycleConfigurationDescription_lifecyclePolicies :: Lens.Lens' LifecycleConfigurationDescription (Prelude.Maybe [LifecyclePolicy])
lifecycleConfigurationDescription_lifecyclePolicies = Lens.lens (\LifecycleConfigurationDescription' {lifecyclePolicies} -> lifecyclePolicies) (\s@LifecycleConfigurationDescription' {} a -> s {lifecyclePolicies = a} :: LifecycleConfigurationDescription) Prelude.. Lens.mapping Lens._Coerce

instance
  Core.FromJSON
    LifecycleConfigurationDescription
  where
  parseJSON =
    Core.withObject
      "LifecycleConfigurationDescription"
      ( \x ->
          LifecycleConfigurationDescription'
            Prelude.<$> ( x Core..:? "LifecyclePolicies"
                            Core..!= Prelude.mempty
                        )
      )

instance
  Prelude.Hashable
    LifecycleConfigurationDescription

instance
  Prelude.NFData
    LifecycleConfigurationDescription
