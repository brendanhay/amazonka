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
-- Module      : Network.AWS.ServiceCatalog.Types.ResourceChange
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.ResourceChange where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.ServiceCatalog.Types.ChangeAction
import Network.AWS.ServiceCatalog.Types.Replacement
import Network.AWS.ServiceCatalog.Types.ResourceAttribute
import Network.AWS.ServiceCatalog.Types.ResourceChangeDetail

-- | Information about a resource change that will occur when a plan is
-- executed.
--
-- /See:/ 'newResourceChange' smart constructor.
data ResourceChange = ResourceChange'
  { -- | The ID of the resource, if it was already created.
    physicalResourceId :: Prelude.Maybe Prelude.Text,
    -- | The type of resource.
    resourceType :: Prelude.Maybe Prelude.Text,
    -- | The change scope.
    scope :: Prelude.Maybe [ResourceAttribute],
    -- | Information about the resource changes.
    details :: Prelude.Maybe [ResourceChangeDetail],
    -- | The ID of the resource, as defined in the CloudFormation template.
    logicalResourceId :: Prelude.Maybe Prelude.Text,
    -- | The change action.
    action :: Prelude.Maybe ChangeAction,
    -- | If the change type is @Modify@, indicates whether the existing resource
    -- is deleted and replaced with a new one.
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
-- 'physicalResourceId', 'resourceChange_physicalResourceId' - The ID of the resource, if it was already created.
--
-- 'resourceType', 'resourceChange_resourceType' - The type of resource.
--
-- 'scope', 'resourceChange_scope' - The change scope.
--
-- 'details', 'resourceChange_details' - Information about the resource changes.
--
-- 'logicalResourceId', 'resourceChange_logicalResourceId' - The ID of the resource, as defined in the CloudFormation template.
--
-- 'action', 'resourceChange_action' - The change action.
--
-- 'replacement', 'resourceChange_replacement' - If the change type is @Modify@, indicates whether the existing resource
-- is deleted and replaced with a new one.
newResourceChange ::
  ResourceChange
newResourceChange =
  ResourceChange'
    { physicalResourceId =
        Prelude.Nothing,
      resourceType = Prelude.Nothing,
      scope = Prelude.Nothing,
      details = Prelude.Nothing,
      logicalResourceId = Prelude.Nothing,
      action = Prelude.Nothing,
      replacement = Prelude.Nothing
    }

-- | The ID of the resource, if it was already created.
resourceChange_physicalResourceId :: Lens.Lens' ResourceChange (Prelude.Maybe Prelude.Text)
resourceChange_physicalResourceId = Lens.lens (\ResourceChange' {physicalResourceId} -> physicalResourceId) (\s@ResourceChange' {} a -> s {physicalResourceId = a} :: ResourceChange)

-- | The type of resource.
resourceChange_resourceType :: Lens.Lens' ResourceChange (Prelude.Maybe Prelude.Text)
resourceChange_resourceType = Lens.lens (\ResourceChange' {resourceType} -> resourceType) (\s@ResourceChange' {} a -> s {resourceType = a} :: ResourceChange)

-- | The change scope.
resourceChange_scope :: Lens.Lens' ResourceChange (Prelude.Maybe [ResourceAttribute])
resourceChange_scope = Lens.lens (\ResourceChange' {scope} -> scope) (\s@ResourceChange' {} a -> s {scope = a} :: ResourceChange) Prelude.. Lens.mapping Prelude._Coerce

-- | Information about the resource changes.
resourceChange_details :: Lens.Lens' ResourceChange (Prelude.Maybe [ResourceChangeDetail])
resourceChange_details = Lens.lens (\ResourceChange' {details} -> details) (\s@ResourceChange' {} a -> s {details = a} :: ResourceChange) Prelude.. Lens.mapping Prelude._Coerce

-- | The ID of the resource, as defined in the CloudFormation template.
resourceChange_logicalResourceId :: Lens.Lens' ResourceChange (Prelude.Maybe Prelude.Text)
resourceChange_logicalResourceId = Lens.lens (\ResourceChange' {logicalResourceId} -> logicalResourceId) (\s@ResourceChange' {} a -> s {logicalResourceId = a} :: ResourceChange)

-- | The change action.
resourceChange_action :: Lens.Lens' ResourceChange (Prelude.Maybe ChangeAction)
resourceChange_action = Lens.lens (\ResourceChange' {action} -> action) (\s@ResourceChange' {} a -> s {action = a} :: ResourceChange)

-- | If the change type is @Modify@, indicates whether the existing resource
-- is deleted and replaced with a new one.
resourceChange_replacement :: Lens.Lens' ResourceChange (Prelude.Maybe Replacement)
resourceChange_replacement = Lens.lens (\ResourceChange' {replacement} -> replacement) (\s@ResourceChange' {} a -> s {replacement = a} :: ResourceChange)

instance Prelude.FromJSON ResourceChange where
  parseJSON =
    Prelude.withObject
      "ResourceChange"
      ( \x ->
          ResourceChange'
            Prelude.<$> (x Prelude..:? "PhysicalResourceId")
            Prelude.<*> (x Prelude..:? "ResourceType")
            Prelude.<*> (x Prelude..:? "Scope" Prelude..!= Prelude.mempty)
            Prelude.<*> (x Prelude..:? "Details" Prelude..!= Prelude.mempty)
            Prelude.<*> (x Prelude..:? "LogicalResourceId")
            Prelude.<*> (x Prelude..:? "Action")
            Prelude.<*> (x Prelude..:? "Replacement")
      )

instance Prelude.Hashable ResourceChange

instance Prelude.NFData ResourceChange
