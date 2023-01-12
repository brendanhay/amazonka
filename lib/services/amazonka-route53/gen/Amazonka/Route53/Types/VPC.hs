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
-- Module      : Amazonka.Route53.Types.VPC
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53.Types.VPC where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Route53.Internal
import Amazonka.Route53.Types.VPCRegion

-- | (Private hosted zones only) A complex type that contains information
-- about an Amazon VPC.
--
-- If you associate a private hosted zone with an Amazon VPC when you make
-- a
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_CreateHostedZone.html CreateHostedZone>
-- request, the following parameters are also required.
--
-- /See:/ 'newVPC' smart constructor.
data VPC = VPC'
  { vPCId :: Prelude.Maybe Prelude.Text,
    -- | (Private hosted zones only) The region that an Amazon VPC was created
    -- in.
    vPCRegion :: Prelude.Maybe VPCRegion
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
-- 'vPCId', 'vpc_vPCId' - Undocumented member.
--
-- 'vPCRegion', 'vpc_vPCRegion' - (Private hosted zones only) The region that an Amazon VPC was created
-- in.
newVPC ::
  VPC
newVPC =
  VPC'
    { vPCId = Prelude.Nothing,
      vPCRegion = Prelude.Nothing
    }

-- | Undocumented member.
vpc_vPCId :: Lens.Lens' VPC (Prelude.Maybe Prelude.Text)
vpc_vPCId = Lens.lens (\VPC' {vPCId} -> vPCId) (\s@VPC' {} a -> s {vPCId = a} :: VPC)

-- | (Private hosted zones only) The region that an Amazon VPC was created
-- in.
vpc_vPCRegion :: Lens.Lens' VPC (Prelude.Maybe VPCRegion)
vpc_vPCRegion = Lens.lens (\VPC' {vPCRegion} -> vPCRegion) (\s@VPC' {} a -> s {vPCRegion = a} :: VPC)

instance Data.FromXML VPC where
  parseXML x =
    VPC'
      Prelude.<$> (x Data..@? "VPCId")
      Prelude.<*> (x Data..@? "VPCRegion")

instance Prelude.Hashable VPC where
  hashWithSalt _salt VPC' {..} =
    _salt `Prelude.hashWithSalt` vPCId
      `Prelude.hashWithSalt` vPCRegion

instance Prelude.NFData VPC where
  rnf VPC' {..} =
    Prelude.rnf vPCId
      `Prelude.seq` Prelude.rnf vPCRegion

instance Data.ToXML VPC where
  toXML VPC' {..} =
    Prelude.mconcat
      [ "VPCId" Data.@= vPCId,
        "VPCRegion" Data.@= vPCRegion
      ]
