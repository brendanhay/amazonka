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
-- Module      : Amazonka.EC2.Types.DeleteLaunchTemplateVersionsResponseErrorItem
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.DeleteLaunchTemplateVersionsResponseErrorItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.ResponseError
import qualified Amazonka.Prelude as Prelude

-- | Describes a launch template version that could not be deleted.
--
-- /See:/ 'newDeleteLaunchTemplateVersionsResponseErrorItem' smart constructor.
data DeleteLaunchTemplateVersionsResponseErrorItem = DeleteLaunchTemplateVersionsResponseErrorItem'
  { -- | The ID of the launch template.
    launchTemplateId :: Prelude.Maybe Prelude.Text,
    -- | The name of the launch template.
    launchTemplateName :: Prelude.Maybe Prelude.Text,
    -- | Information about the error.
    responseError :: Prelude.Maybe ResponseError,
    -- | The version number of the launch template.
    versionNumber :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteLaunchTemplateVersionsResponseErrorItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'launchTemplateId', 'deleteLaunchTemplateVersionsResponseErrorItem_launchTemplateId' - The ID of the launch template.
--
-- 'launchTemplateName', 'deleteLaunchTemplateVersionsResponseErrorItem_launchTemplateName' - The name of the launch template.
--
-- 'responseError', 'deleteLaunchTemplateVersionsResponseErrorItem_responseError' - Information about the error.
--
-- 'versionNumber', 'deleteLaunchTemplateVersionsResponseErrorItem_versionNumber' - The version number of the launch template.
newDeleteLaunchTemplateVersionsResponseErrorItem ::
  DeleteLaunchTemplateVersionsResponseErrorItem
newDeleteLaunchTemplateVersionsResponseErrorItem =
  DeleteLaunchTemplateVersionsResponseErrorItem'
    { launchTemplateId =
        Prelude.Nothing,
      launchTemplateName =
        Prelude.Nothing,
      responseError =
        Prelude.Nothing,
      versionNumber =
        Prelude.Nothing
    }

-- | The ID of the launch template.
deleteLaunchTemplateVersionsResponseErrorItem_launchTemplateId :: Lens.Lens' DeleteLaunchTemplateVersionsResponseErrorItem (Prelude.Maybe Prelude.Text)
deleteLaunchTemplateVersionsResponseErrorItem_launchTemplateId = Lens.lens (\DeleteLaunchTemplateVersionsResponseErrorItem' {launchTemplateId} -> launchTemplateId) (\s@DeleteLaunchTemplateVersionsResponseErrorItem' {} a -> s {launchTemplateId = a} :: DeleteLaunchTemplateVersionsResponseErrorItem)

-- | The name of the launch template.
deleteLaunchTemplateVersionsResponseErrorItem_launchTemplateName :: Lens.Lens' DeleteLaunchTemplateVersionsResponseErrorItem (Prelude.Maybe Prelude.Text)
deleteLaunchTemplateVersionsResponseErrorItem_launchTemplateName = Lens.lens (\DeleteLaunchTemplateVersionsResponseErrorItem' {launchTemplateName} -> launchTemplateName) (\s@DeleteLaunchTemplateVersionsResponseErrorItem' {} a -> s {launchTemplateName = a} :: DeleteLaunchTemplateVersionsResponseErrorItem)

-- | Information about the error.
deleteLaunchTemplateVersionsResponseErrorItem_responseError :: Lens.Lens' DeleteLaunchTemplateVersionsResponseErrorItem (Prelude.Maybe ResponseError)
deleteLaunchTemplateVersionsResponseErrorItem_responseError = Lens.lens (\DeleteLaunchTemplateVersionsResponseErrorItem' {responseError} -> responseError) (\s@DeleteLaunchTemplateVersionsResponseErrorItem' {} a -> s {responseError = a} :: DeleteLaunchTemplateVersionsResponseErrorItem)

-- | The version number of the launch template.
deleteLaunchTemplateVersionsResponseErrorItem_versionNumber :: Lens.Lens' DeleteLaunchTemplateVersionsResponseErrorItem (Prelude.Maybe Prelude.Integer)
deleteLaunchTemplateVersionsResponseErrorItem_versionNumber = Lens.lens (\DeleteLaunchTemplateVersionsResponseErrorItem' {versionNumber} -> versionNumber) (\s@DeleteLaunchTemplateVersionsResponseErrorItem' {} a -> s {versionNumber = a} :: DeleteLaunchTemplateVersionsResponseErrorItem)

instance
  Data.FromXML
    DeleteLaunchTemplateVersionsResponseErrorItem
  where
  parseXML x =
    DeleteLaunchTemplateVersionsResponseErrorItem'
      Prelude.<$> (x Data..@? "launchTemplateId")
      Prelude.<*> (x Data..@? "launchTemplateName")
      Prelude.<*> (x Data..@? "responseError")
      Prelude.<*> (x Data..@? "versionNumber")

instance
  Prelude.Hashable
    DeleteLaunchTemplateVersionsResponseErrorItem
  where
  hashWithSalt
    _salt
    DeleteLaunchTemplateVersionsResponseErrorItem' {..} =
      _salt
        `Prelude.hashWithSalt` launchTemplateId
        `Prelude.hashWithSalt` launchTemplateName
        `Prelude.hashWithSalt` responseError
        `Prelude.hashWithSalt` versionNumber

instance
  Prelude.NFData
    DeleteLaunchTemplateVersionsResponseErrorItem
  where
  rnf
    DeleteLaunchTemplateVersionsResponseErrorItem' {..} =
      Prelude.rnf launchTemplateId `Prelude.seq`
        Prelude.rnf launchTemplateName `Prelude.seq`
          Prelude.rnf responseError `Prelude.seq`
            Prelude.rnf versionNumber
