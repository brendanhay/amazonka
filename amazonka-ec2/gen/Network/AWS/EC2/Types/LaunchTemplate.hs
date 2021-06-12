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

-- | Describes a launch template.
--
-- /See:/ 'newLaunchTemplate' smart constructor.
data LaunchTemplate = LaunchTemplate'
  { -- | The ID of the launch template.
    launchTemplateId :: Core.Maybe Core.Text,
    -- | The name of the launch template.
    launchTemplateName :: Core.Maybe Core.Text,
    -- | The tags for the launch template.
    tags :: Core.Maybe [Tag],
    -- | The time launch template was created.
    createTime :: Core.Maybe Core.ISO8601,
    -- | The principal that created the launch template.
    createdBy :: Core.Maybe Core.Text,
    -- | The version number of the default version of the launch template.
    defaultVersionNumber :: Core.Maybe Core.Integer,
    -- | The version number of the latest version of the launch template.
    latestVersionNumber :: Core.Maybe Core.Integer
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- 'tags', 'launchTemplate_tags' - The tags for the launch template.
--
-- 'createTime', 'launchTemplate_createTime' - The time launch template was created.
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
    { launchTemplateId = Core.Nothing,
      launchTemplateName = Core.Nothing,
      tags = Core.Nothing,
      createTime = Core.Nothing,
      createdBy = Core.Nothing,
      defaultVersionNumber = Core.Nothing,
      latestVersionNumber = Core.Nothing
    }

-- | The ID of the launch template.
launchTemplate_launchTemplateId :: Lens.Lens' LaunchTemplate (Core.Maybe Core.Text)
launchTemplate_launchTemplateId = Lens.lens (\LaunchTemplate' {launchTemplateId} -> launchTemplateId) (\s@LaunchTemplate' {} a -> s {launchTemplateId = a} :: LaunchTemplate)

-- | The name of the launch template.
launchTemplate_launchTemplateName :: Lens.Lens' LaunchTemplate (Core.Maybe Core.Text)
launchTemplate_launchTemplateName = Lens.lens (\LaunchTemplate' {launchTemplateName} -> launchTemplateName) (\s@LaunchTemplate' {} a -> s {launchTemplateName = a} :: LaunchTemplate)

-- | The tags for the launch template.
launchTemplate_tags :: Lens.Lens' LaunchTemplate (Core.Maybe [Tag])
launchTemplate_tags = Lens.lens (\LaunchTemplate' {tags} -> tags) (\s@LaunchTemplate' {} a -> s {tags = a} :: LaunchTemplate) Core.. Lens.mapping Lens._Coerce

-- | The time launch template was created.
launchTemplate_createTime :: Lens.Lens' LaunchTemplate (Core.Maybe Core.UTCTime)
launchTemplate_createTime = Lens.lens (\LaunchTemplate' {createTime} -> createTime) (\s@LaunchTemplate' {} a -> s {createTime = a} :: LaunchTemplate) Core.. Lens.mapping Core._Time

-- | The principal that created the launch template.
launchTemplate_createdBy :: Lens.Lens' LaunchTemplate (Core.Maybe Core.Text)
launchTemplate_createdBy = Lens.lens (\LaunchTemplate' {createdBy} -> createdBy) (\s@LaunchTemplate' {} a -> s {createdBy = a} :: LaunchTemplate)

-- | The version number of the default version of the launch template.
launchTemplate_defaultVersionNumber :: Lens.Lens' LaunchTemplate (Core.Maybe Core.Integer)
launchTemplate_defaultVersionNumber = Lens.lens (\LaunchTemplate' {defaultVersionNumber} -> defaultVersionNumber) (\s@LaunchTemplate' {} a -> s {defaultVersionNumber = a} :: LaunchTemplate)

-- | The version number of the latest version of the launch template.
launchTemplate_latestVersionNumber :: Lens.Lens' LaunchTemplate (Core.Maybe Core.Integer)
launchTemplate_latestVersionNumber = Lens.lens (\LaunchTemplate' {latestVersionNumber} -> latestVersionNumber) (\s@LaunchTemplate' {} a -> s {latestVersionNumber = a} :: LaunchTemplate)

instance Core.FromXML LaunchTemplate where
  parseXML x =
    LaunchTemplate'
      Core.<$> (x Core..@? "launchTemplateId")
      Core.<*> (x Core..@? "launchTemplateName")
      Core.<*> ( x Core..@? "tagSet" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )
      Core.<*> (x Core..@? "createTime")
      Core.<*> (x Core..@? "createdBy")
      Core.<*> (x Core..@? "defaultVersionNumber")
      Core.<*> (x Core..@? "latestVersionNumber")

instance Core.Hashable LaunchTemplate

instance Core.NFData LaunchTemplate
