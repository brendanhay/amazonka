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
-- Module      : Amazonka.AccessAnalyzer.Types.JobError
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AccessAnalyzer.Types.JobError where

import Amazonka.AccessAnalyzer.Types.JobErrorCode
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains the details about the policy generation error.
--
-- /See:/ 'newJobError' smart constructor.
data JobError = JobError'
  { -- | The job error code.
    code :: JobErrorCode,
    -- | Specific information about the error. For example, which service quota
    -- was exceeded or which resource was not found.
    message :: Prelude.Text
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
-- 'code', 'jobError_code' - The job error code.
--
-- 'message', 'jobError_message' - Specific information about the error. For example, which service quota
-- was exceeded or which resource was not found.
newJobError ::
  -- | 'code'
  JobErrorCode ->
  -- | 'message'
  Prelude.Text ->
  JobError
newJobError pCode_ pMessage_ =
  JobError' {code = pCode_, message = pMessage_}

-- | The job error code.
jobError_code :: Lens.Lens' JobError JobErrorCode
jobError_code = Lens.lens (\JobError' {code} -> code) (\s@JobError' {} a -> s {code = a} :: JobError)

-- | Specific information about the error. For example, which service quota
-- was exceeded or which resource was not found.
jobError_message :: Lens.Lens' JobError Prelude.Text
jobError_message = Lens.lens (\JobError' {message} -> message) (\s@JobError' {} a -> s {message = a} :: JobError)

instance Data.FromJSON JobError where
  parseJSON =
    Data.withObject
      "JobError"
      ( \x ->
          JobError'
            Prelude.<$> (x Data..: "code")
            Prelude.<*> (x Data..: "message")
      )

instance Prelude.Hashable JobError where
  hashWithSalt _salt JobError' {..} =
    _salt
      `Prelude.hashWithSalt` code
      `Prelude.hashWithSalt` message

instance Prelude.NFData JobError where
  rnf JobError' {..} =
    Prelude.rnf code `Prelude.seq` Prelude.rnf message
