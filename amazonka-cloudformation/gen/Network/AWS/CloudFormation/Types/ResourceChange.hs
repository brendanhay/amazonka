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
-- Module      : Network.AWS.CloudFormation.Types.ResourceChange
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.ResourceChange where

import Network.AWS.CloudFormation.Types.ChangeAction
import Network.AWS.CloudFormation.Types.ModuleInfo
import Network.AWS.CloudFormation.Types.Replacement
import Network.AWS.CloudFormation.Types.ResourceAttribute
import Network.AWS.CloudFormation.Types.ResourceChangeDetail
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The @ResourceChange@ structure describes the resource and the action
-- that AWS CloudFormation will perform on it if you execute this change
-- set.
--
-- /See:/ 'newResourceChange' smart constructor.
data ResourceChange = ResourceChange'
  { -- | The resource\'s physical ID (resource name). Resources that you are
    -- adding don\'t have physical IDs because they haven\'t been created.
    physicalResourceId :: Core.Maybe Core.Text,
    -- | The type of AWS CloudFormation resource, such as @AWS::S3::Bucket@.
    resourceType :: Core.Maybe Core.Text,
    -- | For the @Modify@ action, indicates which resource attribute is
    -- triggering this update, such as a change in the resource attribute\'s
    -- @Metadata@, @Properties@, or @Tags@.
    scope :: Core.Maybe [ResourceAttribute],
    -- | For the @Modify@ action, a list of @ResourceChangeDetail@ structures
    -- that describes the changes that AWS CloudFormation will make to the
    -- resource.
    details :: Core.Maybe [ResourceChangeDetail],
    -- | Contains information about the module from which the resource was
    -- created, if the resource was created from a module included in the stack
    -- template.
    moduleInfo :: Core.Maybe ModuleInfo,
    -- | The resource\'s logical ID, which is defined in the stack\'s template.
    logicalResourceId :: Core.Maybe Core.Text,
    -- | The change set ID of the nested change set.
    changeSetId :: Core.Maybe Core.Text,
    -- | The action that AWS CloudFormation takes on the resource, such as @Add@
    -- (adds a new resource), @Modify@ (changes a resource), @Remove@ (deletes
    -- a resource), @Import@ (imports a resource), or @Dynamic@ (exact action
    -- for the resource cannot be determined).
    action :: Core.Maybe ChangeAction,
    -- | For the @Modify@ action, indicates whether AWS CloudFormation will
    -- replace the resource by creating a new one and deleting the old one.
    -- This value depends on the value of the @RequiresRecreation@ property in
    -- the @ResourceTargetDefinition@ structure. For example, if the
    -- @RequiresRecreation@ field is @Always@ and the @Evaluation@ field is
    -- @Static@, @Replacement@ is @True@. If the @RequiresRecreation@ field is
    -- @Always@ and the @Evaluation@ field is @Dynamic@, @Replacement@ is
    -- @Conditionally@.
    --
    -- If you have multiple changes with different @RequiresRecreation@ values,
    -- the @Replacement@ value depends on the change with the most impact. A
    -- @RequiresRecreation@ value of @Always@ has the most impact, followed by
    -- @Conditionally@, and then @Never@.
    replacement :: Core.Maybe Replacement
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ResourceChange' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'physicalResourceId', 'resourceChange_physicalResourceId' - The resource\'s physical ID (resource name). Resources that you are
-- adding don\'t have physical IDs because they haven\'t been created.
--
-- 'resourceType', 'resourceChange_resourceType' - The type of AWS CloudFormation resource, such as @AWS::S3::Bucket@.
--
-- 'scope', 'resourceChange_scope' - For the @Modify@ action, indicates which resource attribute is
-- triggering this update, such as a change in the resource attribute\'s
-- @Metadata@, @Properties@, or @Tags@.
--
-- 'details', 'resourceChange_details' - For the @Modify@ action, a list of @ResourceChangeDetail@ structures
-- that describes the changes that AWS CloudFormation will make to the
-- resource.
--
-- 'moduleInfo', 'resourceChange_moduleInfo' - Contains information about the module from which the resource was
-- created, if the resource was created from a module included in the stack
-- template.
--
-- 'logicalResourceId', 'resourceChange_logicalResourceId' - The resource\'s logical ID, which is defined in the stack\'s template.
--
-- 'changeSetId', 'resourceChange_changeSetId' - The change set ID of the nested change set.
--
-- 'action', 'resourceChange_action' - The action that AWS CloudFormation takes on the resource, such as @Add@
-- (adds a new resource), @Modify@ (changes a resource), @Remove@ (deletes
-- a resource), @Import@ (imports a resource), or @Dynamic@ (exact action
-- for the resource cannot be determined).
--
-- 'replacement', 'resourceChange_replacement' - For the @Modify@ action, indicates whether AWS CloudFormation will
-- replace the resource by creating a new one and deleting the old one.
-- This value depends on the value of the @RequiresRecreation@ property in
-- the @ResourceTargetDefinition@ structure. For example, if the
-- @RequiresRecreation@ field is @Always@ and the @Evaluation@ field is
-- @Static@, @Replacement@ is @True@. If the @RequiresRecreation@ field is
-- @Always@ and the @Evaluation@ field is @Dynamic@, @Replacement@ is
-- @Conditionally@.
--
-- If you have multiple changes with different @RequiresRecreation@ values,
-- the @Replacement@ value depends on the change with the most impact. A
-- @RequiresRecreation@ value of @Always@ has the most impact, followed by
-- @Conditionally@, and then @Never@.
newResourceChange ::
  ResourceChange
newResourceChange =
  ResourceChange'
    { physicalResourceId = Core.Nothing,
      resourceType = Core.Nothing,
      scope = Core.Nothing,
      details = Core.Nothing,
      moduleInfo = Core.Nothing,
      logicalResourceId = Core.Nothing,
      changeSetId = Core.Nothing,
      action = Core.Nothing,
      replacement = Core.Nothing
    }

-- | The resource\'s physical ID (resource name). Resources that you are
-- adding don\'t have physical IDs because they haven\'t been created.
resourceChange_physicalResourceId :: Lens.Lens' ResourceChange (Core.Maybe Core.Text)
resourceChange_physicalResourceId = Lens.lens (\ResourceChange' {physicalResourceId} -> physicalResourceId) (\s@ResourceChange' {} a -> s {physicalResourceId = a} :: ResourceChange)

-- | The type of AWS CloudFormation resource, such as @AWS::S3::Bucket@.
resourceChange_resourceType :: Lens.Lens' ResourceChange (Core.Maybe Core.Text)
resourceChange_resourceType = Lens.lens (\ResourceChange' {resourceType} -> resourceType) (\s@ResourceChange' {} a -> s {resourceType = a} :: ResourceChange)

-- | For the @Modify@ action, indicates which resource attribute is
-- triggering this update, such as a change in the resource attribute\'s
-- @Metadata@, @Properties@, or @Tags@.
resourceChange_scope :: Lens.Lens' ResourceChange (Core.Maybe [ResourceAttribute])
resourceChange_scope = Lens.lens (\ResourceChange' {scope} -> scope) (\s@ResourceChange' {} a -> s {scope = a} :: ResourceChange) Core.. Lens.mapping Lens._Coerce

-- | For the @Modify@ action, a list of @ResourceChangeDetail@ structures
-- that describes the changes that AWS CloudFormation will make to the
-- resource.
resourceChange_details :: Lens.Lens' ResourceChange (Core.Maybe [ResourceChangeDetail])
resourceChange_details = Lens.lens (\ResourceChange' {details} -> details) (\s@ResourceChange' {} a -> s {details = a} :: ResourceChange) Core.. Lens.mapping Lens._Coerce

-- | Contains information about the module from which the resource was
-- created, if the resource was created from a module included in the stack
-- template.
resourceChange_moduleInfo :: Lens.Lens' ResourceChange (Core.Maybe ModuleInfo)
resourceChange_moduleInfo = Lens.lens (\ResourceChange' {moduleInfo} -> moduleInfo) (\s@ResourceChange' {} a -> s {moduleInfo = a} :: ResourceChange)

-- | The resource\'s logical ID, which is defined in the stack\'s template.
resourceChange_logicalResourceId :: Lens.Lens' ResourceChange (Core.Maybe Core.Text)
resourceChange_logicalResourceId = Lens.lens (\ResourceChange' {logicalResourceId} -> logicalResourceId) (\s@ResourceChange' {} a -> s {logicalResourceId = a} :: ResourceChange)

-- | The change set ID of the nested change set.
resourceChange_changeSetId :: Lens.Lens' ResourceChange (Core.Maybe Core.Text)
resourceChange_changeSetId = Lens.lens (\ResourceChange' {changeSetId} -> changeSetId) (\s@ResourceChange' {} a -> s {changeSetId = a} :: ResourceChange)

-- | The action that AWS CloudFormation takes on the resource, such as @Add@
-- (adds a new resource), @Modify@ (changes a resource), @Remove@ (deletes
-- a resource), @Import@ (imports a resource), or @Dynamic@ (exact action
-- for the resource cannot be determined).
resourceChange_action :: Lens.Lens' ResourceChange (Core.Maybe ChangeAction)
resourceChange_action = Lens.lens (\ResourceChange' {action} -> action) (\s@ResourceChange' {} a -> s {action = a} :: ResourceChange)

-- | For the @Modify@ action, indicates whether AWS CloudFormation will
-- replace the resource by creating a new one and deleting the old one.
-- This value depends on the value of the @RequiresRecreation@ property in
-- the @ResourceTargetDefinition@ structure. For example, if the
-- @RequiresRecreation@ field is @Always@ and the @Evaluation@ field is
-- @Static@, @Replacement@ is @True@. If the @RequiresRecreation@ field is
-- @Always@ and the @Evaluation@ field is @Dynamic@, @Replacement@ is
-- @Conditionally@.
--
-- If you have multiple changes with different @RequiresRecreation@ values,
-- the @Replacement@ value depends on the change with the most impact. A
-- @RequiresRecreation@ value of @Always@ has the most impact, followed by
-- @Conditionally@, and then @Never@.
resourceChange_replacement :: Lens.Lens' ResourceChange (Core.Maybe Replacement)
resourceChange_replacement = Lens.lens (\ResourceChange' {replacement} -> replacement) (\s@ResourceChange' {} a -> s {replacement = a} :: ResourceChange)

instance Core.FromXML ResourceChange where
  parseXML x =
    ResourceChange'
      Core.<$> (x Core..@? "PhysicalResourceId")
      Core.<*> (x Core..@? "ResourceType")
      Core.<*> ( x Core..@? "Scope" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "member")
               )
      Core.<*> ( x Core..@? "Details" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "member")
               )
      Core.<*> (x Core..@? "ModuleInfo")
      Core.<*> (x Core..@? "LogicalResourceId")
      Core.<*> (x Core..@? "ChangeSetId")
      Core.<*> (x Core..@? "Action")
      Core.<*> (x Core..@? "Replacement")

instance Core.Hashable ResourceChange

instance Core.NFData ResourceChange
