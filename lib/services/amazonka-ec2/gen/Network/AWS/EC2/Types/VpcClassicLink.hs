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
-- Module      : Network.AWS.EC2.Types.VpcClassicLink
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.VpcClassicLink where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.Tag
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes whether a VPC is enabled for ClassicLink.
--
-- /See:/ 'newVpcClassicLink' smart constructor.
data VpcClassicLink = VpcClassicLink'
  { -- | The ID of the VPC.
    vpcId :: Prelude.Maybe Prelude.Text,
    -- | Any tags assigned to the VPC.
    tags :: Prelude.Maybe [Tag],
    -- | Indicates whether the VPC is enabled for ClassicLink.
    classicLinkEnabled :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VpcClassicLink' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vpcId', 'vpcClassicLink_vpcId' - The ID of the VPC.
--
-- 'tags', 'vpcClassicLink_tags' - Any tags assigned to the VPC.
--
-- 'classicLinkEnabled', 'vpcClassicLink_classicLinkEnabled' - Indicates whether the VPC is enabled for ClassicLink.
newVpcClassicLink ::
  VpcClassicLink
newVpcClassicLink =
  VpcClassicLink'
    { vpcId = Prelude.Nothing,
      tags = Prelude.Nothing,
      classicLinkEnabled = Prelude.Nothing
    }

-- | The ID of the VPC.
vpcClassicLink_vpcId :: Lens.Lens' VpcClassicLink (Prelude.Maybe Prelude.Text)
vpcClassicLink_vpcId = Lens.lens (\VpcClassicLink' {vpcId} -> vpcId) (\s@VpcClassicLink' {} a -> s {vpcId = a} :: VpcClassicLink)

-- | Any tags assigned to the VPC.
vpcClassicLink_tags :: Lens.Lens' VpcClassicLink (Prelude.Maybe [Tag])
vpcClassicLink_tags = Lens.lens (\VpcClassicLink' {tags} -> tags) (\s@VpcClassicLink' {} a -> s {tags = a} :: VpcClassicLink) Prelude.. Lens.mapping Lens.coerced

-- | Indicates whether the VPC is enabled for ClassicLink.
vpcClassicLink_classicLinkEnabled :: Lens.Lens' VpcClassicLink (Prelude.Maybe Prelude.Bool)
vpcClassicLink_classicLinkEnabled = Lens.lens (\VpcClassicLink' {classicLinkEnabled} -> classicLinkEnabled) (\s@VpcClassicLink' {} a -> s {classicLinkEnabled = a} :: VpcClassicLink)

instance Core.FromXML VpcClassicLink where
  parseXML x =
    VpcClassicLink'
      Prelude.<$> (x Core..@? "vpcId")
      Prelude.<*> ( x Core..@? "tagSet" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "item")
                  )
      Prelude.<*> (x Core..@? "classicLinkEnabled")

instance Prelude.Hashable VpcClassicLink

instance Prelude.NFData VpcClassicLink
