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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ELB.Types.TagDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ELB.Internal
import Amazonka.ELB.Types.Tag
import qualified Amazonka.Prelude as Prelude

-- | The tags associated with a load balancer.
--
-- /See:/ 'newTagDescription' smart constructor.
data TagDescription = TagDescription'
  { -- | The name of the load balancer.
    loadBalancerName :: Prelude.Maybe Prelude.Text,
    -- | The tags.
    tags :: Prelude.Maybe (Prelude.NonEmpty Tag)
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
-- 'loadBalancerName', 'tagDescription_loadBalancerName' - The name of the load balancer.
--
-- 'tags', 'tagDescription_tags' - The tags.
newTagDescription ::
  TagDescription
newTagDescription =
  TagDescription'
    { loadBalancerName = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | The name of the load balancer.
tagDescription_loadBalancerName :: Lens.Lens' TagDescription (Prelude.Maybe Prelude.Text)
tagDescription_loadBalancerName = Lens.lens (\TagDescription' {loadBalancerName} -> loadBalancerName) (\s@TagDescription' {} a -> s {loadBalancerName = a} :: TagDescription)

-- | The tags.
tagDescription_tags :: Lens.Lens' TagDescription (Prelude.Maybe (Prelude.NonEmpty Tag))
tagDescription_tags = Lens.lens (\TagDescription' {tags} -> tags) (\s@TagDescription' {} a -> s {tags = a} :: TagDescription) Prelude.. Lens.mapping Lens.coerced

instance Data.FromXML TagDescription where
  parseXML x =
    TagDescription'
      Prelude.<$> (x Data..@? "LoadBalancerName")
      Prelude.<*> ( x
                      Data..@? "Tags"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList1 "member")
                  )

instance Prelude.Hashable TagDescription where
  hashWithSalt _salt TagDescription' {..} =
    _salt
      `Prelude.hashWithSalt` loadBalancerName
      `Prelude.hashWithSalt` tags

instance Prelude.NFData TagDescription where
  rnf TagDescription' {..} =
    Prelude.rnf loadBalancerName
      `Prelude.seq` Prelude.rnf tags
