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
-- Module      : Network.AWS.EC2.Types.LaunchTemplate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LaunchTemplate where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.Tag
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes a launch template.
--
-- /See:/ 'newLaunchTemplate' smart constructor.
data LaunchTemplate = LaunchTemplate'
  { -- | The ID of the launch template.
    launchTemplateId :: Prelude.Maybe Prelude.Text,
    -- | The name of the launch template.
    launchTemplateName :: Prelude.Maybe Prelude.Text,
    -- | The time launch template was created.
    createTime :: Prelude.Maybe Core.ISO8601,
    -- | The tags for the launch template.
    tags :: Prelude.Maybe [Tag],
    -- | The principal that created the launch template.
    createdBy :: Prelude.Maybe Prelude.Text,
    -- | The version number of the default version of the launch template.
    defaultVersionNumber :: Prelude.Maybe Prelude.Integer,
    -- | The version number of the latest version of the launch template.
    latestVersionNumber :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LaunchTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'launchTemplateId', 'launchTemplate_launchTemplateId' - The ID of the launch template.
--
-- 'launchTemplateName', 'launchTemplate_launchTemplateName' - The name of the launch template.
--
-- 'createTime', 'launchTemplate_createTime' - The time launch template was created.
--
-- 'tags', 'launchTemplate_tags' - The tags for the launch template.
--
-- 'createdBy', 'launchTemplate_createdBy' - The principal that created the launch template.
--
-- 'defaultVersionNumber', 'launchTemplate_defaultVersionNumber' - The version number of the default version of the launch template.
--
-- 'latestVersionNumber', 'launchTemplate_latestVersionNumber' - The version number of the latest version of the launch template.
newLaunchTemplate ::
  LaunchTemplate
newLaunchTemplate =
  LaunchTemplate'
    { launchTemplateId = Prelude.Nothing,
      launchTemplateName = Prelude.Nothing,
      createTime = Prelude.Nothing,
      tags = Prelude.Nothing,
      createdBy = Prelude.Nothing,
      defaultVersionNumber = Prelude.Nothing,
      latestVersionNumber = Prelude.Nothing
    }

-- | The ID of the launch template.
launchTemplate_launchTemplateId :: Lens.Lens' LaunchTemplate (Prelude.Maybe Prelude.Text)
launchTemplate_launchTemplateId = Lens.lens (\LaunchTemplate' {launchTemplateId} -> launchTemplateId) (\s@LaunchTemplate' {} a -> s {launchTemplateId = a} :: LaunchTemplate)

-- | The name of the launch template.
launchTemplate_launchTemplateName :: Lens.Lens' LaunchTemplate (Prelude.Maybe Prelude.Text)
launchTemplate_launchTemplateName = Lens.lens (\LaunchTemplate' {launchTemplateName} -> launchTemplateName) (\s@LaunchTemplate' {} a -> s {launchTemplateName = a} :: LaunchTemplate)

-- | The time launch template was created.
launchTemplate_createTime :: Lens.Lens' LaunchTemplate (Prelude.Maybe Prelude.UTCTime)
launchTemplate_createTime = Lens.lens (\LaunchTemplate' {createTime} -> createTime) (\s@LaunchTemplate' {} a -> s {createTime = a} :: LaunchTemplate) Prelude.. Lens.mapping Core._Time

-- | The tags for the launch template.
launchTemplate_tags :: Lens.Lens' LaunchTemplate (Prelude.Maybe [Tag])
launchTemplate_tags = Lens.lens (\LaunchTemplate' {tags} -> tags) (\s@LaunchTemplate' {} a -> s {tags = a} :: LaunchTemplate) Prelude.. Lens.mapping Lens._Coerce

-- | The principal that created the launch template.
launchTemplate_createdBy :: Lens.Lens' LaunchTemplate (Prelude.Maybe Prelude.Text)
launchTemplate_createdBy = Lens.lens (\LaunchTemplate' {createdBy} -> createdBy) (\s@LaunchTemplate' {} a -> s {createdBy = a} :: LaunchTemplate)

-- | The version number of the default version of the launch template.
launchTemplate_defaultVersionNumber :: Lens.Lens' LaunchTemplate (Prelude.Maybe Prelude.Integer)
launchTemplate_defaultVersionNumber = Lens.lens (\LaunchTemplate' {defaultVersionNumber} -> defaultVersionNumber) (\s@LaunchTemplate' {} a -> s {defaultVersionNumber = a} :: LaunchTemplate)

-- | The version number of the latest version of the launch template.
launchTemplate_latestVersionNumber :: Lens.Lens' LaunchTemplate (Prelude.Maybe Prelude.Integer)
launchTemplate_latestVersionNumber = Lens.lens (\LaunchTemplate' {latestVersionNumber} -> latestVersionNumber) (\s@LaunchTemplate' {} a -> s {latestVersionNumber = a} :: LaunchTemplate)

instance Core.FromXML LaunchTemplate where
  parseXML x =
    LaunchTemplate'
      Prelude.<$> (x Core..@? "launchTemplateId")
      Prelude.<*> (x Core..@? "launchTemplateName")
      Prelude.<*> (x Core..@? "createTime")
      Prelude.<*> ( x Core..@? "tagSet" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "item")
                  )
      Prelude.<*> (x Core..@? "createdBy")
      Prelude.<*> (x Core..@? "defaultVersionNumber")
      Prelude.<*> (x Core..@? "latestVersionNumber")

instance Prelude.Hashable LaunchTemplate

instance Prelude.NFData LaunchTemplate
