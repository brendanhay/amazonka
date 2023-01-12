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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Transfer.Types.ListedExecution where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Transfer.Types.ExecutionStatus
import Amazonka.Transfer.Types.FileLocation
import Amazonka.Transfer.Types.ServiceMetadata

-- | Returns properties of the execution that is specified.
--
-- /See:/ 'newListedExecution' smart constructor.
data ListedExecution = ListedExecution'
  { -- | A unique identifier for the execution of a workflow.
    executionId :: Prelude.Maybe Prelude.Text,
    -- | A structure that describes the Amazon S3 or EFS file location. This is
    -- the file location when the execution begins: if the file is being
    -- copied, this is the initial (as opposed to destination) file location.
    initialFileLocation :: Prelude.Maybe FileLocation,
    -- | A container object for the session details that are associated with a
    -- workflow.
    serviceMetadata :: Prelude.Maybe ServiceMetadata,
    -- | The status is one of the execution. Can be in progress, completed,
    -- exception encountered, or handling the exception.
    status :: Prelude.Maybe ExecutionStatus
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
-- 'executionId', 'listedExecution_executionId' - A unique identifier for the execution of a workflow.
--
-- 'initialFileLocation', 'listedExecution_initialFileLocation' - A structure that describes the Amazon S3 or EFS file location. This is
-- the file location when the execution begins: if the file is being
-- copied, this is the initial (as opposed to destination) file location.
--
-- 'serviceMetadata', 'listedExecution_serviceMetadata' - A container object for the session details that are associated with a
-- workflow.
--
-- 'status', 'listedExecution_status' - The status is one of the execution. Can be in progress, completed,
-- exception encountered, or handling the exception.
newListedExecution ::
  ListedExecution
newListedExecution =
  ListedExecution'
    { executionId = Prelude.Nothing,
      initialFileLocation = Prelude.Nothing,
      serviceMetadata = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | A unique identifier for the execution of a workflow.
listedExecution_executionId :: Lens.Lens' ListedExecution (Prelude.Maybe Prelude.Text)
listedExecution_executionId = Lens.lens (\ListedExecution' {executionId} -> executionId) (\s@ListedExecution' {} a -> s {executionId = a} :: ListedExecution)

-- | A structure that describes the Amazon S3 or EFS file location. This is
-- the file location when the execution begins: if the file is being
-- copied, this is the initial (as opposed to destination) file location.
listedExecution_initialFileLocation :: Lens.Lens' ListedExecution (Prelude.Maybe FileLocation)
listedExecution_initialFileLocation = Lens.lens (\ListedExecution' {initialFileLocation} -> initialFileLocation) (\s@ListedExecution' {} a -> s {initialFileLocation = a} :: ListedExecution)

-- | A container object for the session details that are associated with a
-- workflow.
listedExecution_serviceMetadata :: Lens.Lens' ListedExecution (Prelude.Maybe ServiceMetadata)
listedExecution_serviceMetadata = Lens.lens (\ListedExecution' {serviceMetadata} -> serviceMetadata) (\s@ListedExecution' {} a -> s {serviceMetadata = a} :: ListedExecution)

-- | The status is one of the execution. Can be in progress, completed,
-- exception encountered, or handling the exception.
listedExecution_status :: Lens.Lens' ListedExecution (Prelude.Maybe ExecutionStatus)
listedExecution_status = Lens.lens (\ListedExecution' {status} -> status) (\s@ListedExecution' {} a -> s {status = a} :: ListedExecution)

instance Data.FromJSON ListedExecution where
  parseJSON =
    Data.withObject
      "ListedExecution"
      ( \x ->
          ListedExecution'
            Prelude.<$> (x Data..:? "ExecutionId")
            Prelude.<*> (x Data..:? "InitialFileLocation")
            Prelude.<*> (x Data..:? "ServiceMetadata")
            Prelude.<*> (x Data..:? "Status")
      )

instance Prelude.Hashable ListedExecution where
  hashWithSalt _salt ListedExecution' {..} =
    _salt `Prelude.hashWithSalt` executionId
      `Prelude.hashWithSalt` initialFileLocation
      `Prelude.hashWithSalt` serviceMetadata
      `Prelude.hashWithSalt` status

instance Prelude.NFData ListedExecution where
  rnf ListedExecution' {..} =
    Prelude.rnf executionId
      `Prelude.seq` Prelude.rnf initialFileLocation
      `Prelude.seq` Prelude.rnf serviceMetadata
      `Prelude.seq` Prelude.rnf status
