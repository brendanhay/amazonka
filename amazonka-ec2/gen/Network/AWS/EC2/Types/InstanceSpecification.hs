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
-- Module      : Network.AWS.EC2.Types.InstanceSpecification
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.InstanceSpecification where

import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The instance details to specify which volumes should be snapshotted.
--
-- /See:/ 'newInstanceSpecification' smart constructor.
data InstanceSpecification = InstanceSpecification'
  { -- | The instance to specify which volumes should be snapshotted.
    instanceId :: Prelude.Maybe Prelude.Text,
    -- | Excludes the root volume from being snapshotted.
    excludeBootVolume :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'InstanceSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'instanceSpecification_instanceId' - The instance to specify which volumes should be snapshotted.
--
-- 'excludeBootVolume', 'instanceSpecification_excludeBootVolume' - Excludes the root volume from being snapshotted.
newInstanceSpecification ::
  InstanceSpecification
newInstanceSpecification =
  InstanceSpecification'
    { instanceId =
        Prelude.Nothing,
      excludeBootVolume = Prelude.Nothing
    }

-- | The instance to specify which volumes should be snapshotted.
instanceSpecification_instanceId :: Lens.Lens' InstanceSpecification (Prelude.Maybe Prelude.Text)
instanceSpecification_instanceId = Lens.lens (\InstanceSpecification' {instanceId} -> instanceId) (\s@InstanceSpecification' {} a -> s {instanceId = a} :: InstanceSpecification)

-- | Excludes the root volume from being snapshotted.
instanceSpecification_excludeBootVolume :: Lens.Lens' InstanceSpecification (Prelude.Maybe Prelude.Bool)
instanceSpecification_excludeBootVolume = Lens.lens (\InstanceSpecification' {excludeBootVolume} -> excludeBootVolume) (\s@InstanceSpecification' {} a -> s {excludeBootVolume = a} :: InstanceSpecification)

instance Prelude.Hashable InstanceSpecification

instance Prelude.NFData InstanceSpecification

instance Prelude.ToQuery InstanceSpecification where
  toQuery InstanceSpecification' {..} =
    Prelude.mconcat
      [ "InstanceId" Prelude.=: instanceId,
        "ExcludeBootVolume" Prelude.=: excludeBootVolume
      ]
