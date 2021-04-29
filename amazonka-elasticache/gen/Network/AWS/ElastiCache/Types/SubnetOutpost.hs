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
-- Module      : Network.AWS.ElastiCache.Types.SubnetOutpost
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.SubnetOutpost where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The ID of the outpost subnet.
--
-- /See:/ 'newSubnetOutpost' smart constructor.
data SubnetOutpost = SubnetOutpost'
  { -- | The outpost ARN of the subnet.
    subnetOutpostArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'SubnetOutpost' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'subnetOutpostArn', 'subnetOutpost_subnetOutpostArn' - The outpost ARN of the subnet.
newSubnetOutpost ::
  SubnetOutpost
newSubnetOutpost =
  SubnetOutpost' {subnetOutpostArn = Prelude.Nothing}

-- | The outpost ARN of the subnet.
subnetOutpost_subnetOutpostArn :: Lens.Lens' SubnetOutpost (Prelude.Maybe Prelude.Text)
subnetOutpost_subnetOutpostArn = Lens.lens (\SubnetOutpost' {subnetOutpostArn} -> subnetOutpostArn) (\s@SubnetOutpost' {} a -> s {subnetOutpostArn = a} :: SubnetOutpost)

instance Prelude.FromXML SubnetOutpost where
  parseXML x =
    SubnetOutpost'
      Prelude.<$> (x Prelude..@? "SubnetOutpostArn")

instance Prelude.Hashable SubnetOutpost

instance Prelude.NFData SubnetOutpost
