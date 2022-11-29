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
-- Module      : Amazonka.Transfer.Types.ListedExecution
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Transfer.Types.ListedExecution where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Transfer.Types.ExecutionStatus
import Amazonka.Transfer.Types.FileLocation
import Amazonka.Transfer.Types.ServiceMetadata

-- | Returns properties of the execution that is specified.
--
-- /See:/ 'newListedExecution' smart constructor.
data ListedExecution = ListedExecution'
  { -- | A container object for the session details that are associated with a
    -- workflow.
    serviceMetadata :: Prelude.Maybe ServiceMetadata,
    -- | A structure that describes the Amazon S3 or EFS file location. This is
    -- the file location when the execution begins: if the file is being
    -- copied, this is the initial (as opposed to destination) file location.
    initialFileLocation :: Prelude.Maybe FileLocation,
    -- | The status is one of the execution. Can be in progress, completed,
    -- exception encountered, or handling the exception.
    status :: Prelude.Maybe ExecutionStatus,
    -- | A unique identifier for the execution of a workflow.
    executionId :: Prelude.Maybe Prelude.Text
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
-- 'serviceMetadata', 'listedExecution_serviceMetadata' - A container object for the session details that are associated with a
-- workflow.
--
-- 'initialFileLocation', 'listedExecution_initialFileLocation' - A structure that describes the Amazon S3 or EFS file location. This is
-- the file location when the execution begins: if the file is being
-- copied, this is the initial (as opposed to destination) file location.
--
-- 'status', 'listedExecution_status' - The status is one of the execution. Can be in progress, completed,
-- exception encountered, or handling the exception.
--
-- 'executionId', 'listedExecution_executionId' - A unique identifier for the execution of a workflow.
newListedExecution ::
  ListedExecution
newListedExecution =
  ListedExecution'
    { serviceMetadata = Prelude.Nothing,
      initialFileLocation = Prelude.Nothing,
      status = Prelude.Nothing,
      executionId = Prelude.Nothing
    }

-- | A container object for the session details that are associated with a
-- workflow.
listedExecution_serviceMetadata :: Lens.Lens' ListedExecution (Prelude.Maybe ServiceMetadata)
listedExecution_serviceMetadata = Lens.lens (\ListedExecution' {serviceMetadata} -> serviceMetadata) (\s@ListedExecution' {} a -> s {serviceMetadata = a} :: ListedExecution)

-- | A structure that describes the Amazon S3 or EFS file location. This is
-- the file location when the execution begins: if the file is being
-- copied, this is the initial (as opposed to destination) file location.
listedExecution_initialFileLocation :: Lens.Lens' ListedExecution (Prelude.Maybe FileLocation)
listedExecution_initialFileLocation = Lens.lens (\ListedExecution' {initialFileLocation} -> initialFileLocation) (\s@ListedExecution' {} a -> s {initialFileLocation = a} :: ListedExecution)

-- | The status is one of the execution. Can be in progress, completed,
-- exception encountered, or handling the exception.
listedExecution_status :: Lens.Lens' ListedExecution (Prelude.Maybe ExecutionStatus)
listedExecution_status = Lens.lens (\ListedExecution' {status} -> status) (\s@ListedExecution' {} a -> s {status = a} :: ListedExecution)

-- | A unique identifier for the execution of a workflow.
listedExecution_executionId :: Lens.Lens' ListedExecution (Prelude.Maybe Prelude.Text)
listedExecution_executionId = Lens.lens (\ListedExecution' {executionId} -> executionId) (\s@ListedExecution' {} a -> s {executionId = a} :: ListedExecution)

instance Core.FromJSON ListedExecution where
  parseJSON =
    Core.withObject
      "ListedExecution"
      ( \x ->
          ListedExecution'
            Prelude.<$> (x Core..:? "ServiceMetadata")
            Prelude.<*> (x Core..:? "InitialFileLocation")
            Prelude.<*> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "ExecutionId")
      )

instance Prelude.Hashable ListedExecution where
  hashWithSalt _salt ListedExecution' {..} =
    _salt `Prelude.hashWithSalt` serviceMetadata
      `Prelude.hashWithSalt` initialFileLocation
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` executionId

instance Prelude.NFData ListedExecution where
  rnf ListedExecution' {..} =
    Prelude.rnf serviceMetadata
      `Prelude.seq` Prelude.rnf initialFileLocation
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf executionId
