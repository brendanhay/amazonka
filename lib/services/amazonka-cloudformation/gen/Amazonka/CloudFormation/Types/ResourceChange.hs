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
-- Module      : Amazonka.CloudFormation.Types.ResourceChange
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFormation.Types.ResourceChange where

import Amazonka.CloudFormation.Types.ChangeAction
import Amazonka.CloudFormation.Types.ModuleInfo
import Amazonka.CloudFormation.Types.Replacement
import Amazonka.CloudFormation.Types.ResourceAttribute
import Amazonka.CloudFormation.Types.ResourceChangeDetail
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The @ResourceChange@ structure describes the resource and the action
-- that CloudFormation will perform on it if you execute this change set.
--
-- /See:/ 'newResourceChange' smart constructor.
data ResourceChange = ResourceChange'
  { -- | The action that CloudFormation takes on the resource, such as @Add@
    -- (adds a new resource), @Modify@ (changes a resource), @Remove@ (deletes
    -- a resource), @Import@ (imports a resource), or @Dynamic@ (exact action
    -- for the resource can\'t be determined).
    action :: Prelude.Maybe ChangeAction,
    -- | The change set ID of the nested change set.
    changeSetId :: Prelude.Maybe Prelude.Text,
    -- | For the @Modify@ action, a list of @ResourceChangeDetail@ structures
    -- that describes the changes that CloudFormation will make to the
    -- resource.
    details :: Prelude.Maybe [ResourceChangeDetail],
    -- | The resource\'s logical ID, which is defined in the stack\'s template.
    logicalResourceId :: Prelude.Maybe Prelude.Text,
    -- | Contains information about the module from which the resource was
    -- created, if the resource was created from a module included in the stack
    -- template.
    moduleInfo :: Prelude.Maybe ModuleInfo,
    -- | The resource\'s physical ID (resource name). Resources that you are
    -- adding don\'t have physical IDs because they haven\'t been created.
    physicalResourceId :: Prelude.Maybe Prelude.Text,
    -- | For the @Modify@ action, indicates whether CloudFormation will replace
    -- the resource by creating a new one and deleting the old one. This value
    -- depends on the value of the @RequiresRecreation@ property in the
    -- @ResourceTargetDefinition@ structure. For example, if the
    -- @RequiresRecreation@ field is @Always@ and the @Evaluation@ field is
    -- @Static@, @Replacement@ is @True@. If the @RequiresRecreation@ field is
    -- @Always@ and the @Evaluation@ field is @Dynamic@, @Replacement@ is
    -- @Conditionally@.
    --
    -- If you have multiple changes with different @RequiresRecreation@ values,
    -- the @Replacement@ value depends on the change with the most impact. A
    -- @RequiresRecreation@ value of @Always@ has the most impact, followed by
    -- @Conditionally@, and then @Never@.
    replacement :: Prelude.Maybe Replacement,
    -- | The type of CloudFormation resource, such as @AWS::S3::Bucket@.
    resourceType :: Prelude.Maybe Prelude.Text,
    -- | For the @Modify@ action, indicates which resource attribute is
    -- triggering this update, such as a change in the resource attribute\'s
    -- @Metadata@, @Properties@, or @Tags@.
    scope :: Prelude.Maybe [ResourceAttribute]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResourceChange' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'action', 'resourceChange_action' - The action that CloudFormation takes on the resource, such as @Add@
-- (adds a new resource), @Modify@ (changes a resource), @Remove@ (deletes
-- a resource), @Import@ (imports a resource), or @Dynamic@ (exact action
-- for the resource can\'t be determined).
--
-- 'changeSetId', 'resourceChange_changeSetId' - The change set ID of the nested change set.
--
-- 'details', 'resourceChange_details' - For the @Modify@ action, a list of @ResourceChangeDetail@ structures
-- that describes the changes that CloudFormation will make to the
-- resource.
--
-- 'logicalResourceId', 'resourceChange_logicalResourceId' - The resource\'s logical ID, which is defined in the stack\'s template.
--
-- 'moduleInfo', 'resourceChange_moduleInfo' - Contains information about the module from which the resource was
-- created, if the resource was created from a module included in the stack
-- template.
--
-- 'physicalResourceId', 'resourceChange_physicalResourceId' - The resource\'s physical ID (resource name). Resources that you are
-- adding don\'t have physical IDs because they haven\'t been created.
--
-- 'replacement', 'resourceChange_replacement' - For the @Modify@ action, indicates whether CloudFormation will replace
-- the resource by creating a new one and deleting the old one. This value
-- depends on the value of the @RequiresRecreation@ property in the
-- @ResourceTargetDefinition@ structure. For example, if the
-- @RequiresRecreation@ field is @Always@ and the @Evaluation@ field is
-- @Static@, @Replacement@ is @True@. If the @RequiresRecreation@ field is
-- @Always@ and the @Evaluation@ field is @Dynamic@, @Replacement@ is
-- @Conditionally@.
--
-- If you have multiple changes with different @RequiresRecreation@ values,
-- the @Replacement@ value depends on the change with the most impact. A
-- @RequiresRecreation@ value of @Always@ has the most impact, followed by
-- @Conditionally@, and then @Never@.
--
-- 'resourceType', 'resourceChange_resourceType' - The type of CloudFormation resource, such as @AWS::S3::Bucket@.
--
-- 'scope', 'resourceChange_scope' - For the @Modify@ action, indicates which resource attribute is
-- triggering this update, such as a change in the resource attribute\'s
-- @Metadata@, @Properties@, or @Tags@.
newResourceChange ::
  ResourceChange
newResourceChange =
  ResourceChange'
    { action = Prelude.Nothing,
      changeSetId = Prelude.Nothing,
      details = Prelude.Nothing,
      logicalResourceId = Prelude.Nothing,
      moduleInfo = Prelude.Nothing,
      physicalResourceId = Prelude.Nothing,
      replacement = Prelude.Nothing,
      resourceType = Prelude.Nothing,
      scope = Prelude.Nothing
    }

-- | The action that CloudFormation takes on the resource, such as @Add@
-- (adds a new resource), @Modify@ (changes a resource), @Remove@ (deletes
-- a resource), @Import@ (imports a resource), or @Dynamic@ (exact action
-- for the resource can\'t be determined).
resourceChange_action :: Lens.Lens' ResourceChange (Prelude.Maybe ChangeAction)
resourceChange_action = Lens.lens (\ResourceChange' {action} -> action) (\s@ResourceChange' {} a -> s {action = a} :: ResourceChange)

-- | The change set ID of the nested change set.
resourceChange_changeSetId :: Lens.Lens' ResourceChange (Prelude.Maybe Prelude.Text)
resourceChange_changeSetId = Lens.lens (\ResourceChange' {changeSetId} -> changeSetId) (\s@ResourceChange' {} a -> s {changeSetId = a} :: ResourceChange)

-- | For the @Modify@ action, a list of @ResourceChangeDetail@ structures
-- that describes the changes that CloudFormation will make to the
-- resource.
resourceChange_details :: Lens.Lens' ResourceChange (Prelude.Maybe [ResourceChangeDetail])
resourceChange_details = Lens.lens (\ResourceChange' {details} -> details) (\s@ResourceChange' {} a -> s {details = a} :: ResourceChange) Prelude.. Lens.mapping Lens.coerced

-- | The resource\'s logical ID, which is defined in the stack\'s template.
resourceChange_logicalResourceId :: Lens.Lens' ResourceChange (Prelude.Maybe Prelude.Text)
resourceChange_logicalResourceId = Lens.lens (\ResourceChange' {logicalResourceId} -> logicalResourceId) (\s@ResourceChange' {} a -> s {logicalResourceId = a} :: ResourceChange)

-- | Contains information about the module from which the resource was
-- created, if the resource was created from a module included in the stack
-- template.
resourceChange_moduleInfo :: Lens.Lens' ResourceChange (Prelude.Maybe ModuleInfo)
resourceChange_moduleInfo = Lens.lens (\ResourceChange' {moduleInfo} -> moduleInfo) (\s@ResourceChange' {} a -> s {moduleInfo = a} :: ResourceChange)

-- | The resource\'s physical ID (resource name). Resources that you are
-- adding don\'t have physical IDs because they haven\'t been created.
resourceChange_physicalResourceId :: Lens.Lens' ResourceChange (Prelude.Maybe Prelude.Text)
resourceChange_physicalResourceId = Lens.lens (\ResourceChange' {physicalResourceId} -> physicalResourceId) (\s@ResourceChange' {} a -> s {physicalResourceId = a} :: ResourceChange)

-- | For the @Modify@ action, indicates whether CloudFormation will replace
-- the resource by creating a new one and deleting the old one. This value
-- depends on the value of the @RequiresRecreation@ property in the
-- @ResourceTargetDefinition@ structure. For example, if the
-- @RequiresRecreation@ field is @Always@ and the @Evaluation@ field is
-- @Static@, @Replacement@ is @True@. If the @RequiresRecreation@ field is
-- @Always@ and the @Evaluation@ field is @Dynamic@, @Replacement@ is
-- @Conditionally@.
--
-- If you have multiple changes with different @RequiresRecreation@ values,
-- the @Replacement@ value depends on the change with the most impact. A
-- @RequiresRecreation@ value of @Always@ has the most impact, followed by
-- @Conditionally@, and then @Never@.
resourceChange_replacement :: Lens.Lens' ResourceChange (Prelude.Maybe Replacement)
resourceChange_replacement = Lens.lens (\ResourceChange' {replacement} -> replacement) (\s@ResourceChange' {} a -> s {replacement = a} :: ResourceChange)

-- | The type of CloudFormation resource, such as @AWS::S3::Bucket@.
resourceChange_resourceType :: Lens.Lens' ResourceChange (Prelude.Maybe Prelude.Text)
resourceChange_resourceType = Lens.lens (\ResourceChange' {resourceType} -> resourceType) (\s@ResourceChange' {} a -> s {resourceType = a} :: ResourceChange)

-- | For the @Modify@ action, indicates which resource attribute is
-- triggering this update, such as a change in the resource attribute\'s
-- @Metadata@, @Properties@, or @Tags@.
resourceChange_scope :: Lens.Lens' ResourceChange (Prelude.Maybe [ResourceAttribute])
resourceChange_scope = Lens.lens (\ResourceChange' {scope} -> scope) (\s@ResourceChange' {} a -> s {scope = a} :: ResourceChange) Prelude.. Lens.mapping Lens.coerced

instance Data.FromXML ResourceChange where
  parseXML x =
    ResourceChange'
      Prelude.<$> (x Data..@? "Action")
      Prelude.<*> (x Data..@? "ChangeSetId")
      Prelude.<*> ( x
                      Data..@? "Details"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "member")
                  )
      Prelude.<*> (x Data..@? "LogicalResourceId")
      Prelude.<*> (x Data..@? "ModuleInfo")
      Prelude.<*> (x Data..@? "PhysicalResourceId")
      Prelude.<*> (x Data..@? "Replacement")
      Prelude.<*> (x Data..@? "ResourceType")
      Prelude.<*> ( x
                      Data..@? "Scope"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "member")
                  )

instance Prelude.Hashable ResourceChange where
  hashWithSalt _salt ResourceChange' {..} =
    _salt
      `Prelude.hashWithSalt` action
      `Prelude.hashWithSalt` changeSetId
      `Prelude.hashWithSalt` details
      `Prelude.hashWithSalt` logicalResourceId
      `Prelude.hashWithSalt` moduleInfo
      `Prelude.hashWithSalt` physicalResourceId
      `Prelude.hashWithSalt` replacement
      `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` scope

instance Prelude.NFData ResourceChange where
  rnf ResourceChange' {..} =
    Prelude.rnf action
      `Prelude.seq` Prelude.rnf changeSetId
      `Prelude.seq` Prelude.rnf details
      `Prelude.seq` Prelude.rnf logicalResourceId
      `Prelude.seq` Prelude.rnf moduleInfo
      `Prelude.seq` Prelude.rnf physicalResourceId
      `Prelude.seq` Prelude.rnf replacement
      `Prelude.seq` Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf scope
