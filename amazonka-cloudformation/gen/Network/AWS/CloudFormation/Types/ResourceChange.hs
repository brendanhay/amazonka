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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The @ResourceChange@ structure describes the resource and the action
-- that AWS CloudFormation will perform on it if you execute this change
-- set.
--
-- /See:/ 'newResourceChange' smart constructor.
data ResourceChange = ResourceChange'
  { -- | The resource\'s physical ID (resource name). Resources that you are
    -- adding don\'t have physical IDs because they haven\'t been created.
    physicalResourceId :: Prelude.Maybe Prelude.Text,
    -- | The type of AWS CloudFormation resource, such as @AWS::S3::Bucket@.
    resourceType :: Prelude.Maybe Prelude.Text,
    -- | For the @Modify@ action, indicates which resource attribute is
    -- triggering this update, such as a change in the resource attribute\'s
    -- @Metadata@, @Properties@, or @Tags@.
    scope :: Prelude.Maybe [ResourceAttribute],
    -- | For the @Modify@ action, a list of @ResourceChangeDetail@ structures
    -- that describes the changes that AWS CloudFormation will make to the
    -- resource.
    details :: Prelude.Maybe [ResourceChangeDetail],
    -- | Contains information about the module from which the resource was
    -- created, if the resource was created from a module included in the stack
    -- template.
    moduleInfo :: Prelude.Maybe ModuleInfo,
    -- | The resource\'s logical ID, which is defined in the stack\'s template.
    logicalResourceId :: Prelude.Maybe Prelude.Text,
    -- | The change set ID of the nested change set.
    changeSetId :: Prelude.Maybe Prelude.Text,
    -- | The action that AWS CloudFormation takes on the resource, such as @Add@
    -- (adds a new resource), @Modify@ (changes a resource), @Remove@ (deletes
    -- a resource), @Import@ (imports a resource), or @Dynamic@ (exact action
    -- for the resource cannot be determined).
    action :: Prelude.Maybe ChangeAction,
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
    replacement :: Prelude.Maybe Replacement
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { physicalResourceId =
        Prelude.Nothing,
      resourceType = Prelude.Nothing,
      scope = Prelude.Nothing,
      details = Prelude.Nothing,
      moduleInfo = Prelude.Nothing,
      logicalResourceId = Prelude.Nothing,
      changeSetId = Prelude.Nothing,
      action = Prelude.Nothing,
      replacement = Prelude.Nothing
    }

-- | The resource\'s physical ID (resource name). Resources that you are
-- adding don\'t have physical IDs because they haven\'t been created.
resourceChange_physicalResourceId :: Lens.Lens' ResourceChange (Prelude.Maybe Prelude.Text)
resourceChange_physicalResourceId = Lens.lens (\ResourceChange' {physicalResourceId} -> physicalResourceId) (\s@ResourceChange' {} a -> s {physicalResourceId = a} :: ResourceChange)

-- | The type of AWS CloudFormation resource, such as @AWS::S3::Bucket@.
resourceChange_resourceType :: Lens.Lens' ResourceChange (Prelude.Maybe Prelude.Text)
resourceChange_resourceType = Lens.lens (\ResourceChange' {resourceType} -> resourceType) (\s@ResourceChange' {} a -> s {resourceType = a} :: ResourceChange)

-- | For the @Modify@ action, indicates which resource attribute is
-- triggering this update, such as a change in the resource attribute\'s
-- @Metadata@, @Properties@, or @Tags@.
resourceChange_scope :: Lens.Lens' ResourceChange (Prelude.Maybe [ResourceAttribute])
resourceChange_scope = Lens.lens (\ResourceChange' {scope} -> scope) (\s@ResourceChange' {} a -> s {scope = a} :: ResourceChange) Prelude.. Lens.mapping Prelude._Coerce

-- | For the @Modify@ action, a list of @ResourceChangeDetail@ structures
-- that describes the changes that AWS CloudFormation will make to the
-- resource.
resourceChange_details :: Lens.Lens' ResourceChange (Prelude.Maybe [ResourceChangeDetail])
resourceChange_details = Lens.lens (\ResourceChange' {details} -> details) (\s@ResourceChange' {} a -> s {details = a} :: ResourceChange) Prelude.. Lens.mapping Prelude._Coerce

-- | Contains information about the module from which the resource was
-- created, if the resource was created from a module included in the stack
-- template.
resourceChange_moduleInfo :: Lens.Lens' ResourceChange (Prelude.Maybe ModuleInfo)
resourceChange_moduleInfo = Lens.lens (\ResourceChange' {moduleInfo} -> moduleInfo) (\s@ResourceChange' {} a -> s {moduleInfo = a} :: ResourceChange)

-- | The resource\'s logical ID, which is defined in the stack\'s template.
resourceChange_logicalResourceId :: Lens.Lens' ResourceChange (Prelude.Maybe Prelude.Text)
resourceChange_logicalResourceId = Lens.lens (\ResourceChange' {logicalResourceId} -> logicalResourceId) (\s@ResourceChange' {} a -> s {logicalResourceId = a} :: ResourceChange)

-- | The change set ID of the nested change set.
resourceChange_changeSetId :: Lens.Lens' ResourceChange (Prelude.Maybe Prelude.Text)
resourceChange_changeSetId = Lens.lens (\ResourceChange' {changeSetId} -> changeSetId) (\s@ResourceChange' {} a -> s {changeSetId = a} :: ResourceChange)

-- | The action that AWS CloudFormation takes on the resource, such as @Add@
-- (adds a new resource), @Modify@ (changes a resource), @Remove@ (deletes
-- a resource), @Import@ (imports a resource), or @Dynamic@ (exact action
-- for the resource cannot be determined).
resourceChange_action :: Lens.Lens' ResourceChange (Prelude.Maybe ChangeAction)
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
resourceChange_replacement :: Lens.Lens' ResourceChange (Prelude.Maybe Replacement)
resourceChange_replacement = Lens.lens (\ResourceChange' {replacement} -> replacement) (\s@ResourceChange' {} a -> s {replacement = a} :: ResourceChange)

instance Prelude.FromXML ResourceChange where
  parseXML x =
    ResourceChange'
      Prelude.<$> (x Prelude..@? "PhysicalResourceId")
      Prelude.<*> (x Prelude..@? "ResourceType")
      Prelude.<*> ( x Prelude..@? "Scope" Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "member")
                  )
      Prelude.<*> ( x Prelude..@? "Details" Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "member")
                  )
      Prelude.<*> (x Prelude..@? "ModuleInfo")
      Prelude.<*> (x Prelude..@? "LogicalResourceId")
      Prelude.<*> (x Prelude..@? "ChangeSetId")
      Prelude.<*> (x Prelude..@? "Action")
      Prelude.<*> (x Prelude..@? "Replacement")

instance Prelude.Hashable ResourceChange

instance Prelude.NFData ResourceChange
