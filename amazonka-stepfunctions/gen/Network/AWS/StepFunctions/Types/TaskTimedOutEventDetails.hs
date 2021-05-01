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
-- Module      : Network.AWS.StepFunctions.Types.TaskTimedOutEventDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StepFunctions.Types.TaskTimedOutEventDetails where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains details about a resource timeout that occurred during an
-- execution.
--
-- /See:/ 'newTaskTimedOutEventDetails' smart constructor.
data TaskTimedOutEventDetails = TaskTimedOutEventDetails'
  { -- | A more detailed explanation of the cause of the failure.
    cause :: Prelude.Maybe (Prelude.Sensitive Prelude.Text),
    -- | The error code of the failure.
    error :: Prelude.Maybe (Prelude.Sensitive Prelude.Text),
    -- | The action of the resource called by a task state.
    resourceType :: Prelude.Text,
    -- | The service name of the resource in a task state.
    resource :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'TaskTimedOutEventDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cause', 'taskTimedOutEventDetails_cause' - A more detailed explanation of the cause of the failure.
--
-- 'error', 'taskTimedOutEventDetails_error' - The error code of the failure.
--
-- 'resourceType', 'taskTimedOutEventDetails_resourceType' - The action of the resource called by a task state.
--
-- 'resource', 'taskTimedOutEventDetails_resource' - The service name of the resource in a task state.
newTaskTimedOutEventDetails ::
  -- | 'resourceType'
  Prelude.Text ->
  -- | 'resource'
  Prelude.Text ->
  TaskTimedOutEventDetails
newTaskTimedOutEventDetails pResourceType_ pResource_ =
  TaskTimedOutEventDetails'
    { cause = Prelude.Nothing,
      error = Prelude.Nothing,
      resourceType = pResourceType_,
      resource = pResource_
    }

-- | A more detailed explanation of the cause of the failure.
taskTimedOutEventDetails_cause :: Lens.Lens' TaskTimedOutEventDetails (Prelude.Maybe Prelude.Text)
taskTimedOutEventDetails_cause = Lens.lens (\TaskTimedOutEventDetails' {cause} -> cause) (\s@TaskTimedOutEventDetails' {} a -> s {cause = a} :: TaskTimedOutEventDetails) Prelude.. Lens.mapping Prelude._Sensitive

-- | The error code of the failure.
taskTimedOutEventDetails_error :: Lens.Lens' TaskTimedOutEventDetails (Prelude.Maybe Prelude.Text)
taskTimedOutEventDetails_error = Lens.lens (\TaskTimedOutEventDetails' {error} -> error) (\s@TaskTimedOutEventDetails' {} a -> s {error = a} :: TaskTimedOutEventDetails) Prelude.. Lens.mapping Prelude._Sensitive

-- | The action of the resource called by a task state.
taskTimedOutEventDetails_resourceType :: Lens.Lens' TaskTimedOutEventDetails Prelude.Text
taskTimedOutEventDetails_resourceType = Lens.lens (\TaskTimedOutEventDetails' {resourceType} -> resourceType) (\s@TaskTimedOutEventDetails' {} a -> s {resourceType = a} :: TaskTimedOutEventDetails)

-- | The service name of the resource in a task state.
taskTimedOutEventDetails_resource :: Lens.Lens' TaskTimedOutEventDetails Prelude.Text
taskTimedOutEventDetails_resource = Lens.lens (\TaskTimedOutEventDetails' {resource} -> resource) (\s@TaskTimedOutEventDetails' {} a -> s {resource = a} :: TaskTimedOutEventDetails)

instance Prelude.FromJSON TaskTimedOutEventDetails where
  parseJSON =
    Prelude.withObject
      "TaskTimedOutEventDetails"
      ( \x ->
          TaskTimedOutEventDetails'
            Prelude.<$> (x Prelude..:? "cause")
            Prelude.<*> (x Prelude..:? "error")
            Prelude.<*> (x Prelude..: "resourceType")
            Prelude.<*> (x Prelude..: "resource")
      )

instance Prelude.Hashable TaskTimedOutEventDetails

instance Prelude.NFData TaskTimedOutEventDetails
