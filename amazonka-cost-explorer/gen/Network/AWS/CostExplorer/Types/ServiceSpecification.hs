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
-- Module      : Network.AWS.CostExplorer.Types.ServiceSpecification
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.ServiceSpecification where

import Network.AWS.CostExplorer.Types.EC2Specification
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Hardware specifications for the service that you want recommendations
-- for.
--
-- /See:/ 'newServiceSpecification' smart constructor.
data ServiceSpecification = ServiceSpecification'
  { -- | The Amazon EC2 hardware specifications that you want AWS to provide
    -- recommendations for.
    eC2Specification :: Prelude.Maybe EC2Specification
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ServiceSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eC2Specification', 'serviceSpecification_eC2Specification' - The Amazon EC2 hardware specifications that you want AWS to provide
-- recommendations for.
newServiceSpecification ::
  ServiceSpecification
newServiceSpecification =
  ServiceSpecification'
    { eC2Specification =
        Prelude.Nothing
    }

-- | The Amazon EC2 hardware specifications that you want AWS to provide
-- recommendations for.
serviceSpecification_eC2Specification :: Lens.Lens' ServiceSpecification (Prelude.Maybe EC2Specification)
serviceSpecification_eC2Specification = Lens.lens (\ServiceSpecification' {eC2Specification} -> eC2Specification) (\s@ServiceSpecification' {} a -> s {eC2Specification = a} :: ServiceSpecification)

instance Prelude.FromJSON ServiceSpecification where
  parseJSON =
    Prelude.withObject
      "ServiceSpecification"
      ( \x ->
          ServiceSpecification'
            Prelude.<$> (x Prelude..:? "EC2Specification")
      )

instance Prelude.Hashable ServiceSpecification

instance Prelude.NFData ServiceSpecification

instance Prelude.ToJSON ServiceSpecification where
  toJSON ServiceSpecification' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("EC2Specification" Prelude..=)
              Prelude.<$> eC2Specification
          ]
      )
