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
-- Module      : Amazonka.StepFunctions.Types.TaskFailedEventDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.StepFunctions.Types.TaskFailedEventDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains details about a task failure event.
--
-- /See:/ 'newTaskFailedEventDetails' smart constructor.
data TaskFailedEventDetails = TaskFailedEventDetails'
  { -- | A more detailed explanation of the cause of the failure.
    cause :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The error code of the failure.
    error :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The service name of the resource in a task state.
    resourceType :: Prelude.Text,
    -- | The action of the resource called by a task state.
    resource :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

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
-- 'resourceType', 'taskFailedEventDetails_resourceType' - The service name of the resource in a task state.
--
-- 'resource', 'taskFailedEventDetails_resource' - The action of the resource called by a task state.
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
taskFailedEventDetails_cause = Lens.lens (\TaskFailedEventDetails' {cause} -> cause) (\s@TaskFailedEventDetails' {} a -> s {cause = a} :: TaskFailedEventDetails) Prelude.. Lens.mapping Data._Sensitive

-- | The error code of the failure.
taskFailedEventDetails_error :: Lens.Lens' TaskFailedEventDetails (Prelude.Maybe Prelude.Text)
taskFailedEventDetails_error = Lens.lens (\TaskFailedEventDetails' {error} -> error) (\s@TaskFailedEventDetails' {} a -> s {error = a} :: TaskFailedEventDetails) Prelude.. Lens.mapping Data._Sensitive

-- | The service name of the resource in a task state.
taskFailedEventDetails_resourceType :: Lens.Lens' TaskFailedEventDetails Prelude.Text
taskFailedEventDetails_resourceType = Lens.lens (\TaskFailedEventDetails' {resourceType} -> resourceType) (\s@TaskFailedEventDetails' {} a -> s {resourceType = a} :: TaskFailedEventDetails)

-- | The action of the resource called by a task state.
taskFailedEventDetails_resource :: Lens.Lens' TaskFailedEventDetails Prelude.Text
taskFailedEventDetails_resource = Lens.lens (\TaskFailedEventDetails' {resource} -> resource) (\s@TaskFailedEventDetails' {} a -> s {resource = a} :: TaskFailedEventDetails)

instance Data.FromJSON TaskFailedEventDetails where
  parseJSON =
    Data.withObject
      "TaskFailedEventDetails"
      ( \x ->
          TaskFailedEventDetails'
            Prelude.<$> (x Data..:? "cause")
            Prelude.<*> (x Data..:? "error")
            Prelude.<*> (x Data..: "resourceType")
            Prelude.<*> (x Data..: "resource")
      )

instance Prelude.Hashable TaskFailedEventDetails where
  hashWithSalt _salt TaskFailedEventDetails' {..} =
    _salt
      `Prelude.hashWithSalt` cause
      `Prelude.hashWithSalt` error
      `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` resource

instance Prelude.NFData TaskFailedEventDetails where
  rnf TaskFailedEventDetails' {..} =
    Prelude.rnf cause
      `Prelude.seq` Prelude.rnf error
      `Prelude.seq` Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf resource
