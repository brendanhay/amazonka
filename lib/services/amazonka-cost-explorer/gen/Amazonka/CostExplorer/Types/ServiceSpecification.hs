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
-- Module      : Amazonka.CostExplorer.Types.ServiceSpecification
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CostExplorer.Types.ServiceSpecification where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.CostExplorer.Types.EC2Specification
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Hardware specifications for the service that you want recommendations
-- for.
--
-- /See:/ 'newServiceSpecification' smart constructor.
data ServiceSpecification = ServiceSpecification'
  { -- | The Amazon EC2 hardware specifications that you want Amazon Web Services
    -- to provide recommendations for.
    eC2Specification :: Prelude.Maybe EC2Specification
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ServiceSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eC2Specification', 'serviceSpecification_eC2Specification' - The Amazon EC2 hardware specifications that you want Amazon Web Services
-- to provide recommendations for.
newServiceSpecification ::
  ServiceSpecification
newServiceSpecification =
  ServiceSpecification'
    { eC2Specification =
        Prelude.Nothing
    }

-- | The Amazon EC2 hardware specifications that you want Amazon Web Services
-- to provide recommendations for.
serviceSpecification_eC2Specification :: Lens.Lens' ServiceSpecification (Prelude.Maybe EC2Specification)
serviceSpecification_eC2Specification = Lens.lens (\ServiceSpecification' {eC2Specification} -> eC2Specification) (\s@ServiceSpecification' {} a -> s {eC2Specification = a} :: ServiceSpecification)

instance Data.FromJSON ServiceSpecification where
  parseJSON =
    Data.withObject
      "ServiceSpecification"
      ( \x ->
          ServiceSpecification'
            Prelude.<$> (x Data..:? "EC2Specification")
      )

instance Prelude.Hashable ServiceSpecification where
  hashWithSalt _salt ServiceSpecification' {..} =
    _salt `Prelude.hashWithSalt` eC2Specification

instance Prelude.NFData ServiceSpecification where
  rnf ServiceSpecification' {..} =
    Prelude.rnf eC2Specification

instance Data.ToJSON ServiceSpecification where
  toJSON ServiceSpecification' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("EC2Specification" Data..=)
              Prelude.<$> eC2Specification
          ]
      )
