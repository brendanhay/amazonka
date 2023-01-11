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
-- Module      : Amazonka.EC2.Types.DeleteLaunchTemplateVersionsResponseSuccessItem
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.DeleteLaunchTemplateVersionsResponseSuccessItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | Describes a launch template version that was successfully deleted.
--
-- /See:/ 'newDeleteLaunchTemplateVersionsResponseSuccessItem' smart constructor.
data DeleteLaunchTemplateVersionsResponseSuccessItem = DeleteLaunchTemplateVersionsResponseSuccessItem'
  { -- | The ID of the launch template.
    launchTemplateId :: Prelude.Maybe Prelude.Text,
    -- | The name of the launch template.
    launchTemplateName :: Prelude.Maybe Prelude.Text,
    -- | The version number of the launch template.
    versionNumber :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteLaunchTemplateVersionsResponseSuccessItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'launchTemplateId', 'deleteLaunchTemplateVersionsResponseSuccessItem_launchTemplateId' - The ID of the launch template.
--
-- 'launchTemplateName', 'deleteLaunchTemplateVersionsResponseSuccessItem_launchTemplateName' - The name of the launch template.
--
-- 'versionNumber', 'deleteLaunchTemplateVersionsResponseSuccessItem_versionNumber' - The version number of the launch template.
newDeleteLaunchTemplateVersionsResponseSuccessItem ::
  DeleteLaunchTemplateVersionsResponseSuccessItem
newDeleteLaunchTemplateVersionsResponseSuccessItem =
  DeleteLaunchTemplateVersionsResponseSuccessItem'
    { launchTemplateId =
        Prelude.Nothing,
      launchTemplateName =
        Prelude.Nothing,
      versionNumber =
        Prelude.Nothing
    }

-- | The ID of the launch template.
deleteLaunchTemplateVersionsResponseSuccessItem_launchTemplateId :: Lens.Lens' DeleteLaunchTemplateVersionsResponseSuccessItem (Prelude.Maybe Prelude.Text)
deleteLaunchTemplateVersionsResponseSuccessItem_launchTemplateId = Lens.lens (\DeleteLaunchTemplateVersionsResponseSuccessItem' {launchTemplateId} -> launchTemplateId) (\s@DeleteLaunchTemplateVersionsResponseSuccessItem' {} a -> s {launchTemplateId = a} :: DeleteLaunchTemplateVersionsResponseSuccessItem)

-- | The name of the launch template.
deleteLaunchTemplateVersionsResponseSuccessItem_launchTemplateName :: Lens.Lens' DeleteLaunchTemplateVersionsResponseSuccessItem (Prelude.Maybe Prelude.Text)
deleteLaunchTemplateVersionsResponseSuccessItem_launchTemplateName = Lens.lens (\DeleteLaunchTemplateVersionsResponseSuccessItem' {launchTemplateName} -> launchTemplateName) (\s@DeleteLaunchTemplateVersionsResponseSuccessItem' {} a -> s {launchTemplateName = a} :: DeleteLaunchTemplateVersionsResponseSuccessItem)

-- | The version number of the launch template.
deleteLaunchTemplateVersionsResponseSuccessItem_versionNumber :: Lens.Lens' DeleteLaunchTemplateVersionsResponseSuccessItem (Prelude.Maybe Prelude.Integer)
deleteLaunchTemplateVersionsResponseSuccessItem_versionNumber = Lens.lens (\DeleteLaunchTemplateVersionsResponseSuccessItem' {versionNumber} -> versionNumber) (\s@DeleteLaunchTemplateVersionsResponseSuccessItem' {} a -> s {versionNumber = a} :: DeleteLaunchTemplateVersionsResponseSuccessItem)

instance
  Data.FromXML
    DeleteLaunchTemplateVersionsResponseSuccessItem
  where
  parseXML x =
    DeleteLaunchTemplateVersionsResponseSuccessItem'
      Prelude.<$> (x Data..@? "launchTemplateId")
        Prelude.<*> (x Data..@? "launchTemplateName")
        Prelude.<*> (x Data..@? "versionNumber")

instance
  Prelude.Hashable
    DeleteLaunchTemplateVersionsResponseSuccessItem
  where
  hashWithSalt
    _salt
    DeleteLaunchTemplateVersionsResponseSuccessItem' {..} =
      _salt `Prelude.hashWithSalt` launchTemplateId
        `Prelude.hashWithSalt` launchTemplateName
        `Prelude.hashWithSalt` versionNumber

instance
  Prelude.NFData
    DeleteLaunchTemplateVersionsResponseSuccessItem
  where
  rnf
    DeleteLaunchTemplateVersionsResponseSuccessItem' {..} =
      Prelude.rnf launchTemplateId
        `Prelude.seq` Prelude.rnf launchTemplateName
        `Prelude.seq` Prelude.rnf versionNumber
