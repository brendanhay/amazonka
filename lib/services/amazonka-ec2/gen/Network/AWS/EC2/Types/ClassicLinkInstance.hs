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
-- Module      : Network.AWS.EC2.Types.ClassicLinkInstance
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ClassicLinkInstance where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.GroupIdentifier
import Network.AWS.EC2.Types.Tag
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes a linked EC2-Classic instance.
--
-- /See:/ 'newClassicLinkInstance' smart constructor.
data ClassicLinkInstance = ClassicLinkInstance'
  { -- | The ID of the instance.
    instanceId :: Prelude.Maybe Prelude.Text,
    -- | A list of security groups.
    groups :: Prelude.Maybe [GroupIdentifier],
    -- | The ID of the VPC.
    vpcId :: Prelude.Maybe Prelude.Text,
    -- | Any tags assigned to the instance.
    tags :: Prelude.Maybe [Tag]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ClassicLinkInstance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'classicLinkInstance_instanceId' - The ID of the instance.
--
-- 'groups', 'classicLinkInstance_groups' - A list of security groups.
--
-- 'vpcId', 'classicLinkInstance_vpcId' - The ID of the VPC.
--
-- 'tags', 'classicLinkInstance_tags' - Any tags assigned to the instance.
newClassicLinkInstance ::
  ClassicLinkInstance
newClassicLinkInstance =
  ClassicLinkInstance'
    { instanceId = Prelude.Nothing,
      groups = Prelude.Nothing,
      vpcId = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | The ID of the instance.
classicLinkInstance_instanceId :: Lens.Lens' ClassicLinkInstance (Prelude.Maybe Prelude.Text)
classicLinkInstance_instanceId = Lens.lens (\ClassicLinkInstance' {instanceId} -> instanceId) (\s@ClassicLinkInstance' {} a -> s {instanceId = a} :: ClassicLinkInstance)

-- | A list of security groups.
classicLinkInstance_groups :: Lens.Lens' ClassicLinkInstance (Prelude.Maybe [GroupIdentifier])
classicLinkInstance_groups = Lens.lens (\ClassicLinkInstance' {groups} -> groups) (\s@ClassicLinkInstance' {} a -> s {groups = a} :: ClassicLinkInstance) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the VPC.
classicLinkInstance_vpcId :: Lens.Lens' ClassicLinkInstance (Prelude.Maybe Prelude.Text)
classicLinkInstance_vpcId = Lens.lens (\ClassicLinkInstance' {vpcId} -> vpcId) (\s@ClassicLinkInstance' {} a -> s {vpcId = a} :: ClassicLinkInstance)

-- | Any tags assigned to the instance.
classicLinkInstance_tags :: Lens.Lens' ClassicLinkInstance (Prelude.Maybe [Tag])
classicLinkInstance_tags = Lens.lens (\ClassicLinkInstance' {tags} -> tags) (\s@ClassicLinkInstance' {} a -> s {tags = a} :: ClassicLinkInstance) Prelude.. Lens.mapping Lens.coerced

instance Core.FromXML ClassicLinkInstance where
  parseXML x =
    ClassicLinkInstance'
      Prelude.<$> (x Core..@? "instanceId")
      Prelude.<*> ( x Core..@? "groupSet" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "item")
                  )
      Prelude.<*> (x Core..@? "vpcId")
      Prelude.<*> ( x Core..@? "tagSet" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "item")
                  )

instance Prelude.Hashable ClassicLinkInstance

instance Prelude.NFData ClassicLinkInstance
