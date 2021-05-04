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
-- Module      : Network.AWS.ELB.Types.TagDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELB.Types.TagDescription where

import Network.AWS.ELB.Internal
import Network.AWS.ELB.Types.Tag
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The tags associated with a load balancer.
--
-- /See:/ 'newTagDescription' smart constructor.
data TagDescription = TagDescription'
  { -- | The tags.
    tags :: Prelude.Maybe (Prelude.NonEmpty Tag),
    -- | The name of the load balancer.
    loadBalancerName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { tags = Prelude.Nothing,
      loadBalancerName = Prelude.Nothing
    }

-- | The tags.
tagDescription_tags :: Lens.Lens' TagDescription (Prelude.Maybe (Prelude.NonEmpty Tag))
tagDescription_tags = Lens.lens (\TagDescription' {tags} -> tags) (\s@TagDescription' {} a -> s {tags = a} :: TagDescription) Prelude.. Lens.mapping Prelude._Coerce

-- | The name of the load balancer.
tagDescription_loadBalancerName :: Lens.Lens' TagDescription (Prelude.Maybe Prelude.Text)
tagDescription_loadBalancerName = Lens.lens (\TagDescription' {loadBalancerName} -> loadBalancerName) (\s@TagDescription' {} a -> s {loadBalancerName = a} :: TagDescription)

instance Prelude.FromXML TagDescription where
  parseXML x =
    TagDescription'
      Prelude.<$> ( x Prelude..@? "Tags" Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList1 "member")
                  )
      Prelude.<*> (x Prelude..@? "LoadBalancerName")

instance Prelude.Hashable TagDescription

instance Prelude.NFData TagDescription
