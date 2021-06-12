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
-- Module      : Network.AWS.ELB.Types.TagDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELB.Types.TagDescription where

import qualified Network.AWS.Core as Core
import Network.AWS.ELB.Internal
import Network.AWS.ELB.Types.Tag
import qualified Network.AWS.Lens as Lens

-- | The tags associated with a load balancer.
--
-- /See:/ 'newTagDescription' smart constructor.
data TagDescription = TagDescription'
  { -- | The tags.
    tags :: Core.Maybe (Core.NonEmpty Tag),
    -- | The name of the load balancer.
    loadBalancerName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'TagDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'tagDescription_tags' - The tags.
--
-- 'loadBalancerName', 'tagDescription_loadBalancerName' - The name of the load balancer.
newTagDescription ::
  TagDescription
newTagDescription =
  TagDescription'
    { tags = Core.Nothing,
      loadBalancerName = Core.Nothing
    }

-- | The tags.
tagDescription_tags :: Lens.Lens' TagDescription (Core.Maybe (Core.NonEmpty Tag))
tagDescription_tags = Lens.lens (\TagDescription' {tags} -> tags) (\s@TagDescription' {} a -> s {tags = a} :: TagDescription) Core.. Lens.mapping Lens._Coerce

-- | The name of the load balancer.
tagDescription_loadBalancerName :: Lens.Lens' TagDescription (Core.Maybe Core.Text)
tagDescription_loadBalancerName = Lens.lens (\TagDescription' {loadBalancerName} -> loadBalancerName) (\s@TagDescription' {} a -> s {loadBalancerName = a} :: TagDescription)

instance Core.FromXML TagDescription where
  parseXML x =
    TagDescription'
      Core.<$> ( x Core..@? "Tags" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList1 "member")
               )
      Core.<*> (x Core..@? "LoadBalancerName")

instance Core.Hashable TagDescription

instance Core.NFData TagDescription
