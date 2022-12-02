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
-- Module      : Amazonka.EC2.Types.LaunchTemplateTagSpecificationRequest
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.LaunchTemplateTagSpecificationRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.ResourceType
import Amazonka.EC2.Types.Tag
import qualified Amazonka.Prelude as Prelude

-- | The tags specification for the resources that are created during
-- instance launch.
--
-- /See:/ 'newLaunchTemplateTagSpecificationRequest' smart constructor.
data LaunchTemplateTagSpecificationRequest = LaunchTemplateTagSpecificationRequest'
  { -- | The tags to apply to the resource.
    tags :: Prelude.Maybe [Tag],
    -- | The type of resource to tag.
    --
    -- The @Valid Values@ are all the resource types that can be tagged.
    -- However, when creating a launch template, you can specify tags for the
    -- following resource types only: @instance@ | @volume@ | @elastic-gpu@ |
    -- @network-interface@ | @spot-instances-request@
    --
    -- To tag a resource after it has been created, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateTags.html CreateTags>.
    resourceType :: Prelude.Maybe ResourceType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LaunchTemplateTagSpecificationRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'launchTemplateTagSpecificationRequest_tags' - The tags to apply to the resource.
--
-- 'resourceType', 'launchTemplateTagSpecificationRequest_resourceType' - The type of resource to tag.
--
-- The @Valid Values@ are all the resource types that can be tagged.
-- However, when creating a launch template, you can specify tags for the
-- following resource types only: @instance@ | @volume@ | @elastic-gpu@ |
-- @network-interface@ | @spot-instances-request@
--
-- To tag a resource after it has been created, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateTags.html CreateTags>.
newLaunchTemplateTagSpecificationRequest ::
  LaunchTemplateTagSpecificationRequest
newLaunchTemplateTagSpecificationRequest =
  LaunchTemplateTagSpecificationRequest'
    { tags =
        Prelude.Nothing,
      resourceType = Prelude.Nothing
    }

-- | The tags to apply to the resource.
launchTemplateTagSpecificationRequest_tags :: Lens.Lens' LaunchTemplateTagSpecificationRequest (Prelude.Maybe [Tag])
launchTemplateTagSpecificationRequest_tags = Lens.lens (\LaunchTemplateTagSpecificationRequest' {tags} -> tags) (\s@LaunchTemplateTagSpecificationRequest' {} a -> s {tags = a} :: LaunchTemplateTagSpecificationRequest) Prelude.. Lens.mapping Lens.coerced

-- | The type of resource to tag.
--
-- The @Valid Values@ are all the resource types that can be tagged.
-- However, when creating a launch template, you can specify tags for the
-- following resource types only: @instance@ | @volume@ | @elastic-gpu@ |
-- @network-interface@ | @spot-instances-request@
--
-- To tag a resource after it has been created, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateTags.html CreateTags>.
launchTemplateTagSpecificationRequest_resourceType :: Lens.Lens' LaunchTemplateTagSpecificationRequest (Prelude.Maybe ResourceType)
launchTemplateTagSpecificationRequest_resourceType = Lens.lens (\LaunchTemplateTagSpecificationRequest' {resourceType} -> resourceType) (\s@LaunchTemplateTagSpecificationRequest' {} a -> s {resourceType = a} :: LaunchTemplateTagSpecificationRequest)

instance
  Prelude.Hashable
    LaunchTemplateTagSpecificationRequest
  where
  hashWithSalt
    _salt
    LaunchTemplateTagSpecificationRequest' {..} =
      _salt `Prelude.hashWithSalt` tags
        `Prelude.hashWithSalt` resourceType

instance
  Prelude.NFData
    LaunchTemplateTagSpecificationRequest
  where
  rnf LaunchTemplateTagSpecificationRequest' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf resourceType

instance
  Data.ToQuery
    LaunchTemplateTagSpecificationRequest
  where
  toQuery LaunchTemplateTagSpecificationRequest' {..} =
    Prelude.mconcat
      [ Data.toQuery
          (Data.toQueryList "Tag" Prelude.<$> tags),
        "ResourceType" Data.=: resourceType
      ]
