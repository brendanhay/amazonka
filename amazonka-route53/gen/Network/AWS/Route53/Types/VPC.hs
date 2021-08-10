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
-- Module      : Network.AWS.Route53.Types.VPC
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53.Types.VPC where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Route53.Internal
import Network.AWS.Route53.Types.VPCRegion

-- | (Private hosted zones only) A complex type that contains information
-- about an Amazon VPC.
--
-- /See:/ 'newVPC' smart constructor.
data VPC = VPC'
  { -- | (Private hosted zones only) The region that an Amazon VPC was created
    -- in.
    vPCRegion :: Prelude.Maybe VPCRegion,
    vPCId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VPC' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vPCRegion', 'vpc_vPCRegion' - (Private hosted zones only) The region that an Amazon VPC was created
-- in.
--
-- 'vPCId', 'vpc_vPCId' - Undocumented member.
newVPC ::
  VPC
newVPC =
  VPC'
    { vPCRegion = Prelude.Nothing,
      vPCId = Prelude.Nothing
    }

-- | (Private hosted zones only) The region that an Amazon VPC was created
-- in.
vpc_vPCRegion :: Lens.Lens' VPC (Prelude.Maybe VPCRegion)
vpc_vPCRegion = Lens.lens (\VPC' {vPCRegion} -> vPCRegion) (\s@VPC' {} a -> s {vPCRegion = a} :: VPC)

-- | Undocumented member.
vpc_vPCId :: Lens.Lens' VPC (Prelude.Maybe Prelude.Text)
vpc_vPCId = Lens.lens (\VPC' {vPCId} -> vPCId) (\s@VPC' {} a -> s {vPCId = a} :: VPC)

instance Core.FromXML VPC where
  parseXML x =
    VPC'
      Prelude.<$> (x Core..@? "VPCRegion")
      Prelude.<*> (x Core..@? "VPCId")

instance Prelude.Hashable VPC

instance Prelude.NFData VPC

instance Core.ToXML VPC where
  toXML VPC' {..} =
    Prelude.mconcat
      [ "VPCRegion" Core.@= vPCRegion,
        "VPCId" Core.@= vPCId
      ]
