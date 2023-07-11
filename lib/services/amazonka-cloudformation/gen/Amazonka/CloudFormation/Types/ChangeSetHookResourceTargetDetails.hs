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
-- Module      : Amazonka.CloudFormation.Types.ChangeSetHookResourceTargetDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFormation.Types.ChangeSetHookResourceTargetDetails where

import Amazonka.CloudFormation.Types.ChangeAction
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies @RESOURCE@ type target details for activated hooks.
--
-- /See:/ 'newChangeSetHookResourceTargetDetails' smart constructor.
data ChangeSetHookResourceTargetDetails = ChangeSetHookResourceTargetDetails'
  { -- | The resource\'s logical ID, which is defined in the stack\'s template.
    logicalResourceId :: Prelude.Maybe Prelude.Text,
    -- | Specifies the action of the resource.
    resourceAction :: Prelude.Maybe ChangeAction,
    -- | The type of CloudFormation resource, such as @AWS::S3::Bucket@.
    resourceType :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ChangeSetHookResourceTargetDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'logicalResourceId', 'changeSetHookResourceTargetDetails_logicalResourceId' - The resource\'s logical ID, which is defined in the stack\'s template.
--
-- 'resourceAction', 'changeSetHookResourceTargetDetails_resourceAction' - Specifies the action of the resource.
--
-- 'resourceType', 'changeSetHookResourceTargetDetails_resourceType' - The type of CloudFormation resource, such as @AWS::S3::Bucket@.
newChangeSetHookResourceTargetDetails ::
  ChangeSetHookResourceTargetDetails
newChangeSetHookResourceTargetDetails =
  ChangeSetHookResourceTargetDetails'
    { logicalResourceId =
        Prelude.Nothing,
      resourceAction = Prelude.Nothing,
      resourceType = Prelude.Nothing
    }

-- | The resource\'s logical ID, which is defined in the stack\'s template.
changeSetHookResourceTargetDetails_logicalResourceId :: Lens.Lens' ChangeSetHookResourceTargetDetails (Prelude.Maybe Prelude.Text)
changeSetHookResourceTargetDetails_logicalResourceId = Lens.lens (\ChangeSetHookResourceTargetDetails' {logicalResourceId} -> logicalResourceId) (\s@ChangeSetHookResourceTargetDetails' {} a -> s {logicalResourceId = a} :: ChangeSetHookResourceTargetDetails)

-- | Specifies the action of the resource.
changeSetHookResourceTargetDetails_resourceAction :: Lens.Lens' ChangeSetHookResourceTargetDetails (Prelude.Maybe ChangeAction)
changeSetHookResourceTargetDetails_resourceAction = Lens.lens (\ChangeSetHookResourceTargetDetails' {resourceAction} -> resourceAction) (\s@ChangeSetHookResourceTargetDetails' {} a -> s {resourceAction = a} :: ChangeSetHookResourceTargetDetails)

-- | The type of CloudFormation resource, such as @AWS::S3::Bucket@.
changeSetHookResourceTargetDetails_resourceType :: Lens.Lens' ChangeSetHookResourceTargetDetails (Prelude.Maybe Prelude.Text)
changeSetHookResourceTargetDetails_resourceType = Lens.lens (\ChangeSetHookResourceTargetDetails' {resourceType} -> resourceType) (\s@ChangeSetHookResourceTargetDetails' {} a -> s {resourceType = a} :: ChangeSetHookResourceTargetDetails)

instance
  Data.FromXML
    ChangeSetHookResourceTargetDetails
  where
  parseXML x =
    ChangeSetHookResourceTargetDetails'
      Prelude.<$> (x Data..@? "LogicalResourceId")
      Prelude.<*> (x Data..@? "ResourceAction")
      Prelude.<*> (x Data..@? "ResourceType")

instance
  Prelude.Hashable
    ChangeSetHookResourceTargetDetails
  where
  hashWithSalt
    _salt
    ChangeSetHookResourceTargetDetails' {..} =
      _salt
        `Prelude.hashWithSalt` logicalResourceId
        `Prelude.hashWithSalt` resourceAction
        `Prelude.hashWithSalt` resourceType

instance
  Prelude.NFData
    ChangeSetHookResourceTargetDetails
  where
  rnf ChangeSetHookResourceTargetDetails' {..} =
    Prelude.rnf logicalResourceId
      `Prelude.seq` Prelude.rnf resourceAction
      `Prelude.seq` Prelude.rnf resourceType
