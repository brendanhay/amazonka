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
-- Module      : Amazonka.DataExchange.Types.JobError
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataExchange.Types.JobError where

import qualified Amazonka.Core as Core
import Amazonka.DataExchange.Types.Code
import Amazonka.DataExchange.Types.Details
import Amazonka.DataExchange.Types.JobErrorLimitName
import Amazonka.DataExchange.Types.JobErrorResourceTypes
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | An error that occurred with the job request.
--
-- /See:/ 'newJobError' smart constructor.
data JobError = JobError'
  { -- | The unique identifier for the resource related to the error.
    resourceId :: Prelude.Maybe Prelude.Text,
    -- | The name of the limit that was reached.
    limitName :: Prelude.Maybe JobErrorLimitName,
    -- | The type of resource related to the error.
    resourceType :: Prelude.Maybe JobErrorResourceTypes,
    -- | The details about the job error.
    details :: Prelude.Maybe Details,
    -- | The value of the exceeded limit.
    limitValue :: Prelude.Maybe Prelude.Double,
    -- | The message related to the job error.
    message :: Prelude.Text,
    -- | The code for the job error.
    code :: Code
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'JobError' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceId', 'jobError_resourceId' - The unique identifier for the resource related to the error.
--
-- 'limitName', 'jobError_limitName' - The name of the limit that was reached.
--
-- 'resourceType', 'jobError_resourceType' - The type of resource related to the error.
--
-- 'details', 'jobError_details' - The details about the job error.
--
-- 'limitValue', 'jobError_limitValue' - The value of the exceeded limit.
--
-- 'message', 'jobError_message' - The message related to the job error.
--
-- 'code', 'jobError_code' - The code for the job error.
newJobError ::
  -- | 'message'
  Prelude.Text ->
  -- | 'code'
  Code ->
  JobError
newJobError pMessage_ pCode_ =
  JobError'
    { resourceId = Prelude.Nothing,
      limitName = Prelude.Nothing,
      resourceType = Prelude.Nothing,
      details = Prelude.Nothing,
      limitValue = Prelude.Nothing,
      message = pMessage_,
      code = pCode_
    }

-- | The unique identifier for the resource related to the error.
jobError_resourceId :: Lens.Lens' JobError (Prelude.Maybe Prelude.Text)
jobError_resourceId = Lens.lens (\JobError' {resourceId} -> resourceId) (\s@JobError' {} a -> s {resourceId = a} :: JobError)

-- | The name of the limit that was reached.
jobError_limitName :: Lens.Lens' JobError (Prelude.Maybe JobErrorLimitName)
jobError_limitName = Lens.lens (\JobError' {limitName} -> limitName) (\s@JobError' {} a -> s {limitName = a} :: JobError)

-- | The type of resource related to the error.
jobError_resourceType :: Lens.Lens' JobError (Prelude.Maybe JobErrorResourceTypes)
jobError_resourceType = Lens.lens (\JobError' {resourceType} -> resourceType) (\s@JobError' {} a -> s {resourceType = a} :: JobError)

-- | The details about the job error.
jobError_details :: Lens.Lens' JobError (Prelude.Maybe Details)
jobError_details = Lens.lens (\JobError' {details} -> details) (\s@JobError' {} a -> s {details = a} :: JobError)

-- | The value of the exceeded limit.
jobError_limitValue :: Lens.Lens' JobError (Prelude.Maybe Prelude.Double)
jobError_limitValue = Lens.lens (\JobError' {limitValue} -> limitValue) (\s@JobError' {} a -> s {limitValue = a} :: JobError)

-- | The message related to the job error.
jobError_message :: Lens.Lens' JobError Prelude.Text
jobError_message = Lens.lens (\JobError' {message} -> message) (\s@JobError' {} a -> s {message = a} :: JobError)

-- | The code for the job error.
jobError_code :: Lens.Lens' JobError Code
jobError_code = Lens.lens (\JobError' {code} -> code) (\s@JobError' {} a -> s {code = a} :: JobError)

instance Core.FromJSON JobError where
  parseJSON =
    Core.withObject
      "JobError"
      ( \x ->
          JobError'
            Prelude.<$> (x Core..:? "ResourceId")
            Prelude.<*> (x Core..:? "LimitName")
            Prelude.<*> (x Core..:? "ResourceType")
            Prelude.<*> (x Core..:? "Details")
            Prelude.<*> (x Core..:? "LimitValue")
            Prelude.<*> (x Core..: "Message")
            Prelude.<*> (x Core..: "Code")
      )

instance Prelude.Hashable JobError

instance Prelude.NFData JobError
