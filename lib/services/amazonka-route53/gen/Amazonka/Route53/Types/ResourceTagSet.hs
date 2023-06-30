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
-- Module      : Amazonka.Route53.Types.ResourceTagSet
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53.Types.ResourceTagSet where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Route53.Internal
import Amazonka.Route53.Types.Tag
import Amazonka.Route53.Types.TagResourceType

-- | A complex type containing a resource and its associated tags.
--
-- /See:/ 'newResourceTagSet' smart constructor.
data ResourceTagSet = ResourceTagSet'
  { -- | The ID for the specified resource.
    resourceId :: Prelude.Maybe Prelude.Text,
    -- | The type of the resource.
    --
    -- -   The resource type for health checks is @healthcheck@.
    --
    -- -   The resource type for hosted zones is @hostedzone@.
    resourceType :: Prelude.Maybe TagResourceType,
    -- | The tags associated with the specified resource.
    tags :: Prelude.Maybe (Prelude.NonEmpty Tag)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResourceTagSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceId', 'resourceTagSet_resourceId' - The ID for the specified resource.
--
-- 'resourceType', 'resourceTagSet_resourceType' - The type of the resource.
--
-- -   The resource type for health checks is @healthcheck@.
--
-- -   The resource type for hosted zones is @hostedzone@.
--
-- 'tags', 'resourceTagSet_tags' - The tags associated with the specified resource.
newResourceTagSet ::
  ResourceTagSet
newResourceTagSet =
  ResourceTagSet'
    { resourceId = Prelude.Nothing,
      resourceType = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | The ID for the specified resource.
resourceTagSet_resourceId :: Lens.Lens' ResourceTagSet (Prelude.Maybe Prelude.Text)
resourceTagSet_resourceId = Lens.lens (\ResourceTagSet' {resourceId} -> resourceId) (\s@ResourceTagSet' {} a -> s {resourceId = a} :: ResourceTagSet)

-- | The type of the resource.
--
-- -   The resource type for health checks is @healthcheck@.
--
-- -   The resource type for hosted zones is @hostedzone@.
resourceTagSet_resourceType :: Lens.Lens' ResourceTagSet (Prelude.Maybe TagResourceType)
resourceTagSet_resourceType = Lens.lens (\ResourceTagSet' {resourceType} -> resourceType) (\s@ResourceTagSet' {} a -> s {resourceType = a} :: ResourceTagSet)

-- | The tags associated with the specified resource.
resourceTagSet_tags :: Lens.Lens' ResourceTagSet (Prelude.Maybe (Prelude.NonEmpty Tag))
resourceTagSet_tags = Lens.lens (\ResourceTagSet' {tags} -> tags) (\s@ResourceTagSet' {} a -> s {tags = a} :: ResourceTagSet) Prelude.. Lens.mapping Lens.coerced

instance Data.FromXML ResourceTagSet where
  parseXML x =
    ResourceTagSet'
      Prelude.<$> (x Data..@? "ResourceId")
      Prelude.<*> (x Data..@? "ResourceType")
      Prelude.<*> ( x
                      Data..@? "Tags"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList1 "Tag")
                  )

instance Prelude.Hashable ResourceTagSet where
  hashWithSalt _salt ResourceTagSet' {..} =
    _salt
      `Prelude.hashWithSalt` resourceId
      `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` tags

instance Prelude.NFData ResourceTagSet where
  rnf ResourceTagSet' {..} =
    Prelude.rnf resourceId
      `Prelude.seq` Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf tags
