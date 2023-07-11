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
-- Module      : Amazonka.EC2.Types.LaunchTemplateTagSpecification
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.LaunchTemplateTagSpecification where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.ResourceType
import Amazonka.EC2.Types.Tag
import qualified Amazonka.Prelude as Prelude

-- | The tags specification for the launch template.
--
-- /See:/ 'newLaunchTemplateTagSpecification' smart constructor.
data LaunchTemplateTagSpecification = LaunchTemplateTagSpecification'
  { -- | The type of resource to tag.
    resourceType :: Prelude.Maybe ResourceType,
    -- | The tags for the resource.
    tags :: Prelude.Maybe [Tag]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LaunchTemplateTagSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceType', 'launchTemplateTagSpecification_resourceType' - The type of resource to tag.
--
-- 'tags', 'launchTemplateTagSpecification_tags' - The tags for the resource.
newLaunchTemplateTagSpecification ::
  LaunchTemplateTagSpecification
newLaunchTemplateTagSpecification =
  LaunchTemplateTagSpecification'
    { resourceType =
        Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | The type of resource to tag.
launchTemplateTagSpecification_resourceType :: Lens.Lens' LaunchTemplateTagSpecification (Prelude.Maybe ResourceType)
launchTemplateTagSpecification_resourceType = Lens.lens (\LaunchTemplateTagSpecification' {resourceType} -> resourceType) (\s@LaunchTemplateTagSpecification' {} a -> s {resourceType = a} :: LaunchTemplateTagSpecification)

-- | The tags for the resource.
launchTemplateTagSpecification_tags :: Lens.Lens' LaunchTemplateTagSpecification (Prelude.Maybe [Tag])
launchTemplateTagSpecification_tags = Lens.lens (\LaunchTemplateTagSpecification' {tags} -> tags) (\s@LaunchTemplateTagSpecification' {} a -> s {tags = a} :: LaunchTemplateTagSpecification) Prelude.. Lens.mapping Lens.coerced

instance Data.FromXML LaunchTemplateTagSpecification where
  parseXML x =
    LaunchTemplateTagSpecification'
      Prelude.<$> (x Data..@? "resourceType")
      Prelude.<*> ( x
                      Data..@? "tagSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )

instance
  Prelude.Hashable
    LaunchTemplateTagSpecification
  where
  hashWithSalt
    _salt
    LaunchTemplateTagSpecification' {..} =
      _salt
        `Prelude.hashWithSalt` resourceType
        `Prelude.hashWithSalt` tags

instance
  Prelude.NFData
    LaunchTemplateTagSpecification
  where
  rnf LaunchTemplateTagSpecification' {..} =
    Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf tags
