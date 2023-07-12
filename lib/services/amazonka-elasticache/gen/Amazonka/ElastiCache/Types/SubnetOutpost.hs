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
-- Module      : Amazonka.ElastiCache.Types.SubnetOutpost
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElastiCache.Types.SubnetOutpost where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The ID of the outpost subnet.
--
-- /See:/ 'newSubnetOutpost' smart constructor.
data SubnetOutpost = SubnetOutpost'
  { -- | The outpost ARN of the subnet.
    subnetOutpostArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Data.FromXML SubnetOutpost where
  parseXML x =
    SubnetOutpost'
      Prelude.<$> (x Data..@? "SubnetOutpostArn")

instance Prelude.Hashable SubnetOutpost where
  hashWithSalt _salt SubnetOutpost' {..} =
    _salt `Prelude.hashWithSalt` subnetOutpostArn

instance Prelude.NFData SubnetOutpost where
  rnf SubnetOutpost' {..} = Prelude.rnf subnetOutpostArn
