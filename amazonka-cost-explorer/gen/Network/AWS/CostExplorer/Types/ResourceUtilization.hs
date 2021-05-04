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
-- Module      : Network.AWS.CostExplorer.Types.ResourceUtilization
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.ResourceUtilization where

import Network.AWS.CostExplorer.Types.EC2ResourceUtilization
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Resource utilization of current resource.
--
-- /See:/ 'newResourceUtilization' smart constructor.
data ResourceUtilization = ResourceUtilization'
  { -- | Utilization of current Amazon EC2 instance.
    eC2ResourceUtilization :: Prelude.Maybe EC2ResourceUtilization
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ResourceUtilization' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eC2ResourceUtilization', 'resourceUtilization_eC2ResourceUtilization' - Utilization of current Amazon EC2 instance.
newResourceUtilization ::
  ResourceUtilization
newResourceUtilization =
  ResourceUtilization'
    { eC2ResourceUtilization =
        Prelude.Nothing
    }

-- | Utilization of current Amazon EC2 instance.
resourceUtilization_eC2ResourceUtilization :: Lens.Lens' ResourceUtilization (Prelude.Maybe EC2ResourceUtilization)
resourceUtilization_eC2ResourceUtilization = Lens.lens (\ResourceUtilization' {eC2ResourceUtilization} -> eC2ResourceUtilization) (\s@ResourceUtilization' {} a -> s {eC2ResourceUtilization = a} :: ResourceUtilization)

instance Prelude.FromJSON ResourceUtilization where
  parseJSON =
    Prelude.withObject
      "ResourceUtilization"
      ( \x ->
          ResourceUtilization'
            Prelude.<$> (x Prelude..:? "EC2ResourceUtilization")
      )

instance Prelude.Hashable ResourceUtilization

instance Prelude.NFData ResourceUtilization
