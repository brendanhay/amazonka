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
-- Module      : Network.AWS.StepFunctions.Types.TaskFailedEventDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StepFunctions.Types.TaskFailedEventDetails where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains details about a task failure event.
--
-- /See:/ 'newTaskFailedEventDetails' smart constructor.
data TaskFailedEventDetails = TaskFailedEventDetails'
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
-- Create a value of 'TaskFailedEventDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cause', 'taskFailedEventDetails_cause' - A more detailed explanation of the cause of the failure.
--
-- 'error', 'taskFailedEventDetails_error' - The error code of the failure.
--
-- 'resourceType', 'taskFailedEventDetails_resourceType' - The action of the resource called by a task state.
--
-- 'resource', 'taskFailedEventDetails_resource' - The service name of the resource in a task state.
newTaskFailedEventDetails ::
  -- | 'resourceType'
  Prelude.Text ->
  -- | 'resource'
  Prelude.Text ->
  TaskFailedEventDetails
newTaskFailedEventDetails pResourceType_ pResource_ =
  TaskFailedEventDetails'
    { cause = Prelude.Nothing,
      error = Prelude.Nothing,
      resourceType = pResourceType_,
      resource = pResource_
    }

-- | A more detailed explanation of the cause of the failure.
taskFailedEventDetails_cause :: Lens.Lens' TaskFailedEventDetails (Prelude.Maybe Prelude.Text)
taskFailedEventDetails_cause = Lens.lens (\TaskFailedEventDetails' {cause} -> cause) (\s@TaskFailedEventDetails' {} a -> s {cause = a} :: TaskFailedEventDetails) Prelude.. Lens.mapping Prelude._Sensitive

-- | The error code of the failure.
taskFailedEventDetails_error :: Lens.Lens' TaskFailedEventDetails (Prelude.Maybe Prelude.Text)
taskFailedEventDetails_error = Lens.lens (\TaskFailedEventDetails' {error} -> error) (\s@TaskFailedEventDetails' {} a -> s {error = a} :: TaskFailedEventDetails) Prelude.. Lens.mapping Prelude._Sensitive

-- | The action of the resource called by a task state.
taskFailedEventDetails_resourceType :: Lens.Lens' TaskFailedEventDetails Prelude.Text
taskFailedEventDetails_resourceType = Lens.lens (\TaskFailedEventDetails' {resourceType} -> resourceType) (\s@TaskFailedEventDetails' {} a -> s {resourceType = a} :: TaskFailedEventDetails)

-- | The service name of the resource in a task state.
taskFailedEventDetails_resource :: Lens.Lens' TaskFailedEventDetails Prelude.Text
taskFailedEventDetails_resource = Lens.lens (\TaskFailedEventDetails' {resource} -> resource) (\s@TaskFailedEventDetails' {} a -> s {resource = a} :: TaskFailedEventDetails)

instance Prelude.FromJSON TaskFailedEventDetails where
  parseJSON =
    Prelude.withObject
      "TaskFailedEventDetails"
      ( \x ->
          TaskFailedEventDetails'
            Prelude.<$> (x Prelude..:? "cause")
            Prelude.<*> (x Prelude..:? "error")
            Prelude.<*> (x Prelude..: "resourceType")
            Prelude.<*> (x Prelude..: "resource")
      )

instance Prelude.Hashable TaskFailedEventDetails

instance Prelude.NFData TaskFailedEventDetails
