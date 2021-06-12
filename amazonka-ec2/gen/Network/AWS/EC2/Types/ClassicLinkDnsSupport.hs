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
-- Module      : Network.AWS.EC2.Types.ClassicLinkDnsSupport
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ClassicLinkDnsSupport where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens

-- | Describes the ClassicLink DNS support status of a VPC.
--
-- /See:/ 'newClassicLinkDnsSupport' smart constructor.
data ClassicLinkDnsSupport = ClassicLinkDnsSupport'
  { -- | Indicates whether ClassicLink DNS support is enabled for the VPC.
    classicLinkDnsSupported :: Core.Maybe Core.Bool,
    -- | The ID of the VPC.
    vpcId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ClassicLinkDnsSupport' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'classicLinkDnsSupported', 'classicLinkDnsSupport_classicLinkDnsSupported' - Indicates whether ClassicLink DNS support is enabled for the VPC.
--
-- 'vpcId', 'classicLinkDnsSupport_vpcId' - The ID of the VPC.
newClassicLinkDnsSupport ::
  ClassicLinkDnsSupport
newClassicLinkDnsSupport =
  ClassicLinkDnsSupport'
    { classicLinkDnsSupported =
        Core.Nothing,
      vpcId = Core.Nothing
    }

-- | Indicates whether ClassicLink DNS support is enabled for the VPC.
classicLinkDnsSupport_classicLinkDnsSupported :: Lens.Lens' ClassicLinkDnsSupport (Core.Maybe Core.Bool)
classicLinkDnsSupport_classicLinkDnsSupported = Lens.lens (\ClassicLinkDnsSupport' {classicLinkDnsSupported} -> classicLinkDnsSupported) (\s@ClassicLinkDnsSupport' {} a -> s {classicLinkDnsSupported = a} :: ClassicLinkDnsSupport)

-- | The ID of the VPC.
classicLinkDnsSupport_vpcId :: Lens.Lens' ClassicLinkDnsSupport (Core.Maybe Core.Text)
classicLinkDnsSupport_vpcId = Lens.lens (\ClassicLinkDnsSupport' {vpcId} -> vpcId) (\s@ClassicLinkDnsSupport' {} a -> s {vpcId = a} :: ClassicLinkDnsSupport)

instance Core.FromXML ClassicLinkDnsSupport where
  parseXML x =
    ClassicLinkDnsSupport'
      Core.<$> (x Core..@? "classicLinkDnsSupported")
      Core.<*> (x Core..@? "vpcId")

instance Core.Hashable ClassicLinkDnsSupport

instance Core.NFData ClassicLinkDnsSupport
