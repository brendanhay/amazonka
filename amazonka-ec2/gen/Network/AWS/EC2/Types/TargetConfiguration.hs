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
-- Module      : Network.AWS.EC2.Types.TargetConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TargetConfiguration where

import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about the Convertible Reserved Instance offering.
--
-- /See:/ 'newTargetConfiguration' smart constructor.
data TargetConfiguration = TargetConfiguration'
  { -- | The ID of the Convertible Reserved Instance offering.
    offeringId :: Prelude.Maybe Prelude.Text,
    -- | The number of instances the Convertible Reserved Instance offering can
    -- be applied to. This parameter is reserved and cannot be specified in a
    -- request
    instanceCount :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'TargetConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'offeringId', 'targetConfiguration_offeringId' - The ID of the Convertible Reserved Instance offering.
--
-- 'instanceCount', 'targetConfiguration_instanceCount' - The number of instances the Convertible Reserved Instance offering can
-- be applied to. This parameter is reserved and cannot be specified in a
-- request
newTargetConfiguration ::
  TargetConfiguration
newTargetConfiguration =
  TargetConfiguration'
    { offeringId = Prelude.Nothing,
      instanceCount = Prelude.Nothing
    }

-- | The ID of the Convertible Reserved Instance offering.
targetConfiguration_offeringId :: Lens.Lens' TargetConfiguration (Prelude.Maybe Prelude.Text)
targetConfiguration_offeringId = Lens.lens (\TargetConfiguration' {offeringId} -> offeringId) (\s@TargetConfiguration' {} a -> s {offeringId = a} :: TargetConfiguration)

-- | The number of instances the Convertible Reserved Instance offering can
-- be applied to. This parameter is reserved and cannot be specified in a
-- request
targetConfiguration_instanceCount :: Lens.Lens' TargetConfiguration (Prelude.Maybe Prelude.Int)
targetConfiguration_instanceCount = Lens.lens (\TargetConfiguration' {instanceCount} -> instanceCount) (\s@TargetConfiguration' {} a -> s {instanceCount = a} :: TargetConfiguration)

instance Prelude.FromXML TargetConfiguration where
  parseXML x =
    TargetConfiguration'
      Prelude.<$> (x Prelude..@? "offeringId")
      Prelude.<*> (x Prelude..@? "instanceCount")

instance Prelude.Hashable TargetConfiguration

instance Prelude.NFData TargetConfiguration
