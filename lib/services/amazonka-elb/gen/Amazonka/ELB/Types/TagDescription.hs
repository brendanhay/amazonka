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
-- Module      : Amazonka.ELB.Types.TagDescription
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ELB.Types.TagDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.ELB.Internal
import Amazonka.ELB.Types.Tag
import qualified Amazonka.Prelude as Prelude

-- | The tags associated with a load balancer.
--
-- /See:/ 'newTagDescription' smart constructor.
data TagDescription = TagDescription'
  { -- | The tags.
    tags :: Prelude.Maybe (Prelude.NonEmpty Tag),
    -- | The name of the load balancer.
    loadBalancerName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
tagDescription_tags = Lens.lens (\TagDescription' {tags} -> tags) (\s@TagDescription' {} a -> s {tags = a} :: TagDescription) Prelude.. Lens.mapping Lens.coerced

-- | The name of the load balancer.
tagDescription_loadBalancerName :: Lens.Lens' TagDescription (Prelude.Maybe Prelude.Text)
tagDescription_loadBalancerName = Lens.lens (\TagDescription' {loadBalancerName} -> loadBalancerName) (\s@TagDescription' {} a -> s {loadBalancerName = a} :: TagDescription)

instance Core.FromXML TagDescription where
  parseXML x =
    TagDescription'
      Prelude.<$> ( x Core..@? "Tags" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList1 "member")
                  )
      Prelude.<*> (x Core..@? "LoadBalancerName")

instance Prelude.Hashable TagDescription where
  hashWithSalt _salt TagDescription' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` loadBalancerName

instance Prelude.NFData TagDescription where
  rnf TagDescription' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf loadBalancerName
