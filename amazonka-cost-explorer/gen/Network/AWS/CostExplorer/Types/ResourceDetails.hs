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
-- Module      : Network.AWS.CostExplorer.Types.ResourceDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.ResourceDetails where

import Network.AWS.CostExplorer.Types.EC2ResourceDetails
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Details on the resource.
--
-- /See:/ 'newResourceDetails' smart constructor.
data ResourceDetails = ResourceDetails'
  { -- | Details on the Amazon EC2 resource.
    eC2ResourceDetails :: Prelude.Maybe EC2ResourceDetails
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ResourceDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eC2ResourceDetails', 'resourceDetails_eC2ResourceDetails' - Details on the Amazon EC2 resource.
newResourceDetails ::
  ResourceDetails
newResourceDetails =
  ResourceDetails'
    { eC2ResourceDetails =
        Prelude.Nothing
    }

-- | Details on the Amazon EC2 resource.
resourceDetails_eC2ResourceDetails :: Lens.Lens' ResourceDetails (Prelude.Maybe EC2ResourceDetails)
resourceDetails_eC2ResourceDetails = Lens.lens (\ResourceDetails' {eC2ResourceDetails} -> eC2ResourceDetails) (\s@ResourceDetails' {} a -> s {eC2ResourceDetails = a} :: ResourceDetails)

instance Prelude.FromJSON ResourceDetails where
  parseJSON =
    Prelude.withObject
      "ResourceDetails"
      ( \x ->
          ResourceDetails'
            Prelude.<$> (x Prelude..:? "EC2ResourceDetails")
      )

instance Prelude.Hashable ResourceDetails

instance Prelude.NFData ResourceDetails
