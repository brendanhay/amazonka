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
-- Module      : Network.AWS.SWF.Types.WorkflowType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.WorkflowType where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Represents a workflow type.
--
-- /See:/ 'newWorkflowType' smart constructor.
data WorkflowType = WorkflowType'
  { -- | The name of the workflow type.
    --
    -- The combination of workflow type name and version must be unique with in
    -- a domain.
    name :: Core.Text,
    -- | The version of the workflow type.
    --
    -- The combination of workflow type name and version must be unique with in
    -- a domain.
    version :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'WorkflowType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'workflowType_name' - The name of the workflow type.
--
-- The combination of workflow type name and version must be unique with in
-- a domain.
--
-- 'version', 'workflowType_version' - The version of the workflow type.
--
-- The combination of workflow type name and version must be unique with in
-- a domain.
newWorkflowType ::
  -- | 'name'
  Core.Text ->
  -- | 'version'
  Core.Text ->
  WorkflowType
newWorkflowType pName_ pVersion_ =
  WorkflowType' {name = pName_, version = pVersion_}

-- | The name of the workflow type.
--
-- The combination of workflow type name and version must be unique with in
-- a domain.
workflowType_name :: Lens.Lens' WorkflowType Core.Text
workflowType_name = Lens.lens (\WorkflowType' {name} -> name) (\s@WorkflowType' {} a -> s {name = a} :: WorkflowType)

-- | The version of the workflow type.
--
-- The combination of workflow type name and version must be unique with in
-- a domain.
workflowType_version :: Lens.Lens' WorkflowType Core.Text
workflowType_version = Lens.lens (\WorkflowType' {version} -> version) (\s@WorkflowType' {} a -> s {version = a} :: WorkflowType)

instance Core.FromJSON WorkflowType where
  parseJSON =
    Core.withObject
      "WorkflowType"
      ( \x ->
          WorkflowType'
            Core.<$> (x Core..: "name") Core.<*> (x Core..: "version")
      )

instance Core.Hashable WorkflowType

instance Core.NFData WorkflowType

instance Core.ToJSON WorkflowType where
  toJSON WorkflowType' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("name" Core..= name),
            Core.Just ("version" Core..= version)
          ]
      )
