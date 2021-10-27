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
-- Module      : Network.AWS.Transfer.Types.ListedExecution
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Transfer.Types.ListedExecution where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Transfer.Types.ExecutionStatus
import Network.AWS.Transfer.Types.FileLocation
import Network.AWS.Transfer.Types.ServiceMetadata

-- | Returns properties of the execution that is specified.
--
-- /See:/ 'newListedExecution' smart constructor.
data ListedExecution = ListedExecution'
  { -- | The status is one of the execution. Can be in progress, completed,
    -- exception encountered, or handling the exception.
    status :: Prelude.Maybe ExecutionStatus,
    -- | A unique identifier for the execution of a workflow.
    executionId :: Prelude.Maybe Prelude.Text,
    -- | A structure that describes the Amazon S3 or EFS file location. This is
    -- the file location when the execution begins: if the file is being
    -- copied, this is the initial (as opposed to destination) file location.
    initialFileLocation :: Prelude.Maybe FileLocation,
    -- | A container object for the session details associated with a workflow.
    serviceMetadata :: Prelude.Maybe ServiceMetadata
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListedExecution' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'listedExecution_status' - The status is one of the execution. Can be in progress, completed,
-- exception encountered, or handling the exception.
--
-- 'executionId', 'listedExecution_executionId' - A unique identifier for the execution of a workflow.
--
-- 'initialFileLocation', 'listedExecution_initialFileLocation' - A structure that describes the Amazon S3 or EFS file location. This is
-- the file location when the execution begins: if the file is being
-- copied, this is the initial (as opposed to destination) file location.
--
-- 'serviceMetadata', 'listedExecution_serviceMetadata' - A container object for the session details associated with a workflow.
newListedExecution ::
  ListedExecution
newListedExecution =
  ListedExecution'
    { status = Prelude.Nothing,
      executionId = Prelude.Nothing,
      initialFileLocation = Prelude.Nothing,
      serviceMetadata = Prelude.Nothing
    }

-- | The status is one of the execution. Can be in progress, completed,
-- exception encountered, or handling the exception.
listedExecution_status :: Lens.Lens' ListedExecution (Prelude.Maybe ExecutionStatus)
listedExecution_status = Lens.lens (\ListedExecution' {status} -> status) (\s@ListedExecution' {} a -> s {status = a} :: ListedExecution)

-- | A unique identifier for the execution of a workflow.
listedExecution_executionId :: Lens.Lens' ListedExecution (Prelude.Maybe Prelude.Text)
listedExecution_executionId = Lens.lens (\ListedExecution' {executionId} -> executionId) (\s@ListedExecution' {} a -> s {executionId = a} :: ListedExecution)

-- | A structure that describes the Amazon S3 or EFS file location. This is
-- the file location when the execution begins: if the file is being
-- copied, this is the initial (as opposed to destination) file location.
listedExecution_initialFileLocation :: Lens.Lens' ListedExecution (Prelude.Maybe FileLocation)
listedExecution_initialFileLocation = Lens.lens (\ListedExecution' {initialFileLocation} -> initialFileLocation) (\s@ListedExecution' {} a -> s {initialFileLocation = a} :: ListedExecution)

-- | A container object for the session details associated with a workflow.
listedExecution_serviceMetadata :: Lens.Lens' ListedExecution (Prelude.Maybe ServiceMetadata)
listedExecution_serviceMetadata = Lens.lens (\ListedExecution' {serviceMetadata} -> serviceMetadata) (\s@ListedExecution' {} a -> s {serviceMetadata = a} :: ListedExecution)

instance Core.FromJSON ListedExecution where
  parseJSON =
    Core.withObject
      "ListedExecution"
      ( \x ->
          ListedExecution'
            Prelude.<$> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "ExecutionId")
            Prelude.<*> (x Core..:? "InitialFileLocation")
            Prelude.<*> (x Core..:? "ServiceMetadata")
      )

instance Prelude.Hashable ListedExecution

instance Prelude.NFData ListedExecution
