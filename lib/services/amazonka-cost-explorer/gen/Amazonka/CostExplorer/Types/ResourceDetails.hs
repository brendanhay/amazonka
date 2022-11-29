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
-- Module      : Amazonka.CostExplorer.Types.ResourceDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CostExplorer.Types.ResourceDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.CostExplorer.Types.EC2ResourceDetails
import qualified Amazonka.Prelude as Prelude

-- | Details for the resource.
--
-- /See:/ 'newResourceDetails' smart constructor.
data ResourceDetails = ResourceDetails'
  { -- | Details for the Amazon EC2 resource.
    eC2ResourceDetails :: Prelude.Maybe EC2ResourceDetails
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResourceDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eC2ResourceDetails', 'resourceDetails_eC2ResourceDetails' - Details for the Amazon EC2 resource.
newResourceDetails ::
  ResourceDetails
newResourceDetails =
  ResourceDetails'
    { eC2ResourceDetails =
        Prelude.Nothing
    }

-- | Details for the Amazon EC2 resource.
resourceDetails_eC2ResourceDetails :: Lens.Lens' ResourceDetails (Prelude.Maybe EC2ResourceDetails)
resourceDetails_eC2ResourceDetails = Lens.lens (\ResourceDetails' {eC2ResourceDetails} -> eC2ResourceDetails) (\s@ResourceDetails' {} a -> s {eC2ResourceDetails = a} :: ResourceDetails)

instance Core.FromJSON ResourceDetails where
  parseJSON =
    Core.withObject
      "ResourceDetails"
      ( \x ->
          ResourceDetails'
            Prelude.<$> (x Core..:? "EC2ResourceDetails")
      )

instance Prelude.Hashable ResourceDetails where
  hashWithSalt _salt ResourceDetails' {..} =
    _salt `Prelude.hashWithSalt` eC2ResourceDetails

instance Prelude.NFData ResourceDetails where
  rnf ResourceDetails' {..} =
    Prelude.rnf eC2ResourceDetails
