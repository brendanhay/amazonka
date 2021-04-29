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
-- Module      : Network.AWS.EC2.Types.DeleteLaunchTemplateVersionsResponseErrorItem
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.DeleteLaunchTemplateVersionsResponseErrorItem where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.ResponseError
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes a launch template version that could not be deleted.
--
-- /See:/ 'newDeleteLaunchTemplateVersionsResponseErrorItem' smart constructor.
data DeleteLaunchTemplateVersionsResponseErrorItem = DeleteLaunchTemplateVersionsResponseErrorItem'
  { -- | Information about the error.
    responseError :: Prelude.Maybe ResponseError,
    -- | The ID of the launch template.
    launchTemplateId :: Prelude.Maybe Prelude.Text,
    -- | The name of the launch template.
    launchTemplateName :: Prelude.Maybe Prelude.Text,
    -- | The version number of the launch template.
    versionNumber :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      launchTemplateId =
        Prelude.Nothing,
      launchTemplateName =
        Prelude.Nothing,
      versionNumber =
        Prelude.Nothing
    }

-- | Information about the error.
deleteLaunchTemplateVersionsResponseErrorItem_responseError :: Lens.Lens' DeleteLaunchTemplateVersionsResponseErrorItem (Prelude.Maybe ResponseError)
deleteLaunchTemplateVersionsResponseErrorItem_responseError = Lens.lens (\DeleteLaunchTemplateVersionsResponseErrorItem' {responseError} -> responseError) (\s@DeleteLaunchTemplateVersionsResponseErrorItem' {} a -> s {responseError = a} :: DeleteLaunchTemplateVersionsResponseErrorItem)

-- | The ID of the launch template.
deleteLaunchTemplateVersionsResponseErrorItem_launchTemplateId :: Lens.Lens' DeleteLaunchTemplateVersionsResponseErrorItem (Prelude.Maybe Prelude.Text)
deleteLaunchTemplateVersionsResponseErrorItem_launchTemplateId = Lens.lens (\DeleteLaunchTemplateVersionsResponseErrorItem' {launchTemplateId} -> launchTemplateId) (\s@DeleteLaunchTemplateVersionsResponseErrorItem' {} a -> s {launchTemplateId = a} :: DeleteLaunchTemplateVersionsResponseErrorItem)

-- | The name of the launch template.
deleteLaunchTemplateVersionsResponseErrorItem_launchTemplateName :: Lens.Lens' DeleteLaunchTemplateVersionsResponseErrorItem (Prelude.Maybe Prelude.Text)
deleteLaunchTemplateVersionsResponseErrorItem_launchTemplateName = Lens.lens (\DeleteLaunchTemplateVersionsResponseErrorItem' {launchTemplateName} -> launchTemplateName) (\s@DeleteLaunchTemplateVersionsResponseErrorItem' {} a -> s {launchTemplateName = a} :: DeleteLaunchTemplateVersionsResponseErrorItem)

-- | The version number of the launch template.
deleteLaunchTemplateVersionsResponseErrorItem_versionNumber :: Lens.Lens' DeleteLaunchTemplateVersionsResponseErrorItem (Prelude.Maybe Prelude.Integer)
deleteLaunchTemplateVersionsResponseErrorItem_versionNumber = Lens.lens (\DeleteLaunchTemplateVersionsResponseErrorItem' {versionNumber} -> versionNumber) (\s@DeleteLaunchTemplateVersionsResponseErrorItem' {} a -> s {versionNumber = a} :: DeleteLaunchTemplateVersionsResponseErrorItem)

instance
  Prelude.FromXML
    DeleteLaunchTemplateVersionsResponseErrorItem
  where
  parseXML x =
    DeleteLaunchTemplateVersionsResponseErrorItem'
      Prelude.<$> (x Prelude..@? "responseError")
        Prelude.<*> (x Prelude..@? "launchTemplateId")
        Prelude.<*> (x Prelude..@? "launchTemplateName")
        Prelude.<*> (x Prelude..@? "versionNumber")

instance
  Prelude.Hashable
    DeleteLaunchTemplateVersionsResponseErrorItem

instance
  Prelude.NFData
    DeleteLaunchTemplateVersionsResponseErrorItem
