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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.LaunchTemplateTagSpecificationRequest where

import qualified Amazonka.Core as Core
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.ResourceType
import Amazonka.EC2.Types.Tag
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The tags specification for the launch template.
--
-- /See:/ 'newLaunchTemplateTagSpecificationRequest' smart constructor.
data LaunchTemplateTagSpecificationRequest = LaunchTemplateTagSpecificationRequest'
  { -- | The type of resource to tag. Currently, the resource types that support
    -- tagging on creation are @instance@ and @volume@. To tag a resource after
    -- it has been created, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateTags.html CreateTags>.
    resourceType :: Prelude.Maybe ResourceType,
    -- | The tags to apply to the resource.
    tags :: Prelude.Maybe [Tag]
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
-- 'resourceType', 'launchTemplateTagSpecificationRequest_resourceType' - The type of resource to tag. Currently, the resource types that support
-- tagging on creation are @instance@ and @volume@. To tag a resource after
-- it has been created, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateTags.html CreateTags>.
--
-- 'tags', 'launchTemplateTagSpecificationRequest_tags' - The tags to apply to the resource.
newLaunchTemplateTagSpecificationRequest ::
  LaunchTemplateTagSpecificationRequest
newLaunchTemplateTagSpecificationRequest =
  LaunchTemplateTagSpecificationRequest'
    { resourceType =
        Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | The type of resource to tag. Currently, the resource types that support
-- tagging on creation are @instance@ and @volume@. To tag a resource after
-- it has been created, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateTags.html CreateTags>.
launchTemplateTagSpecificationRequest_resourceType :: Lens.Lens' LaunchTemplateTagSpecificationRequest (Prelude.Maybe ResourceType)
launchTemplateTagSpecificationRequest_resourceType = Lens.lens (\LaunchTemplateTagSpecificationRequest' {resourceType} -> resourceType) (\s@LaunchTemplateTagSpecificationRequest' {} a -> s {resourceType = a} :: LaunchTemplateTagSpecificationRequest)

-- | The tags to apply to the resource.
launchTemplateTagSpecificationRequest_tags :: Lens.Lens' LaunchTemplateTagSpecificationRequest (Prelude.Maybe [Tag])
launchTemplateTagSpecificationRequest_tags = Lens.lens (\LaunchTemplateTagSpecificationRequest' {tags} -> tags) (\s@LaunchTemplateTagSpecificationRequest' {} a -> s {tags = a} :: LaunchTemplateTagSpecificationRequest) Prelude.. Lens.mapping Lens.coerced

instance
  Prelude.Hashable
    LaunchTemplateTagSpecificationRequest
  where
  hashWithSalt
    _salt
    LaunchTemplateTagSpecificationRequest' {..} =
      _salt `Prelude.hashWithSalt` resourceType
        `Prelude.hashWithSalt` tags

instance
  Prelude.NFData
    LaunchTemplateTagSpecificationRequest
  where
  rnf LaunchTemplateTagSpecificationRequest' {..} =
    Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf tags

instance
  Core.ToQuery
    LaunchTemplateTagSpecificationRequest
  where
  toQuery LaunchTemplateTagSpecificationRequest' {..} =
    Prelude.mconcat
      [ "ResourceType" Core.=: resourceType,
        Core.toQuery
          (Core.toQueryList "Tag" Prelude.<$> tags)
      ]
