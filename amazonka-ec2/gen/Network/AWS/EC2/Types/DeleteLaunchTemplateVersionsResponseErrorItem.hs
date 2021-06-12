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
-- Module      : Network.AWS.EC2.Types.DeleteLaunchTemplateVersionsResponseErrorItem
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.DeleteLaunchTemplateVersionsResponseErrorItem where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.ResponseError
import qualified Network.AWS.Lens as Lens

-- | Describes a launch template version that could not be deleted.
--
-- /See:/ 'newDeleteLaunchTemplateVersionsResponseErrorItem' smart constructor.
data DeleteLaunchTemplateVersionsResponseErrorItem = DeleteLaunchTemplateVersionsResponseErrorItem'
  { -- | Information about the error.
    responseError :: Core.Maybe ResponseError,
    -- | The ID of the launch template.
    launchTemplateId :: Core.Maybe Core.Text,
    -- | The name of the launch template.
    launchTemplateName :: Core.Maybe Core.Text,
    -- | The version number of the launch template.
    versionNumber :: Core.Maybe Core.Integer
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteLaunchTemplateVersionsResponseErrorItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'responseError', 'deleteLaunchTemplateVersionsResponseErrorItem_responseError' - Information about the error.
--
-- 'launchTemplateId', 'deleteLaunchTemplateVersionsResponseErrorItem_launchTemplateId' - The ID of the launch template.
--
-- 'launchTemplateName', 'deleteLaunchTemplateVersionsResponseErrorItem_launchTemplateName' - The name of the launch template.
--
-- 'versionNumber', 'deleteLaunchTemplateVersionsResponseErrorItem_versionNumber' - The version number of the launch template.
newDeleteLaunchTemplateVersionsResponseErrorItem ::
  DeleteLaunchTemplateVersionsResponseErrorItem
newDeleteLaunchTemplateVersionsResponseErrorItem =
  DeleteLaunchTemplateVersionsResponseErrorItem'
    { responseError =
        Core.Nothing,
      launchTemplateId =
        Core.Nothing,
      launchTemplateName =
        Core.Nothing,
      versionNumber = Core.Nothing
    }

-- | Information about the error.
deleteLaunchTemplateVersionsResponseErrorItem_responseError :: Lens.Lens' DeleteLaunchTemplateVersionsResponseErrorItem (Core.Maybe ResponseError)
deleteLaunchTemplateVersionsResponseErrorItem_responseError = Lens.lens (\DeleteLaunchTemplateVersionsResponseErrorItem' {responseError} -> responseError) (\s@DeleteLaunchTemplateVersionsResponseErrorItem' {} a -> s {responseError = a} :: DeleteLaunchTemplateVersionsResponseErrorItem)

-- | The ID of the launch template.
deleteLaunchTemplateVersionsResponseErrorItem_launchTemplateId :: Lens.Lens' DeleteLaunchTemplateVersionsResponseErrorItem (Core.Maybe Core.Text)
deleteLaunchTemplateVersionsResponseErrorItem_launchTemplateId = Lens.lens (\DeleteLaunchTemplateVersionsResponseErrorItem' {launchTemplateId} -> launchTemplateId) (\s@DeleteLaunchTemplateVersionsResponseErrorItem' {} a -> s {launchTemplateId = a} :: DeleteLaunchTemplateVersionsResponseErrorItem)

-- | The name of the launch template.
deleteLaunchTemplateVersionsResponseErrorItem_launchTemplateName :: Lens.Lens' DeleteLaunchTemplateVersionsResponseErrorItem (Core.Maybe Core.Text)
deleteLaunchTemplateVersionsResponseErrorItem_launchTemplateName = Lens.lens (\DeleteLaunchTemplateVersionsResponseErrorItem' {launchTemplateName} -> launchTemplateName) (\s@DeleteLaunchTemplateVersionsResponseErrorItem' {} a -> s {launchTemplateName = a} :: DeleteLaunchTemplateVersionsResponseErrorItem)

-- | The version number of the launch template.
deleteLaunchTemplateVersionsResponseErrorItem_versionNumber :: Lens.Lens' DeleteLaunchTemplateVersionsResponseErrorItem (Core.Maybe Core.Integer)
deleteLaunchTemplateVersionsResponseErrorItem_versionNumber = Lens.lens (\DeleteLaunchTemplateVersionsResponseErrorItem' {versionNumber} -> versionNumber) (\s@DeleteLaunchTemplateVersionsResponseErrorItem' {} a -> s {versionNumber = a} :: DeleteLaunchTemplateVersionsResponseErrorItem)

instance
  Core.FromXML
    DeleteLaunchTemplateVersionsResponseErrorItem
  where
  parseXML x =
    DeleteLaunchTemplateVersionsResponseErrorItem'
      Core.<$> (x Core..@? "responseError")
        Core.<*> (x Core..@? "launchTemplateId")
        Core.<*> (x Core..@? "launchTemplateName")
        Core.<*> (x Core..@? "versionNumber")

instance
  Core.Hashable
    DeleteLaunchTemplateVersionsResponseErrorItem

instance
  Core.NFData
    DeleteLaunchTemplateVersionsResponseErrorItem
