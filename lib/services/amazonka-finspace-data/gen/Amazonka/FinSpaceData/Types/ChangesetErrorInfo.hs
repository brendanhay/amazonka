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
-- Module      : Amazonka.FinSpaceData.Types.ChangesetErrorInfo
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FinSpaceData.Types.ChangesetErrorInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FinSpaceData.Types.ErrorCategory
import qualified Amazonka.Prelude as Prelude

-- | The structure with error messages.
--
-- /See:/ 'newChangesetErrorInfo' smart constructor.
data ChangesetErrorInfo = ChangesetErrorInfo'
  { -- | The category of the error.
    --
    -- -   @VALIDATION@ – The inputs to this request are invalid.
    --
    -- -   @SERVICE_QUOTA_EXCEEDED@ – Service quotas have been exceeded. Please
    --     contact AWS support to increase quotas.
    --
    -- -   @ACCESS_DENIED@ – Missing required permission to perform this
    --     request.
    --
    -- -   @RESOURCE_NOT_FOUND@ – One or more inputs to this request were not
    --     found.
    --
    -- -   @THROTTLING@ – The system temporarily lacks sufficient resources to
    --     process the request.
    --
    -- -   @INTERNAL_SERVICE_EXCEPTION@ – An internal service error has
    --     occurred.
    --
    -- -   @CANCELLED@ – Cancelled.
    --
    -- -   @USER_RECOVERABLE@ – A user recoverable error has occurred.
    errorCategory :: Prelude.Maybe ErrorCategory,
    -- | The text of the error message.
    errorMessage :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ChangesetErrorInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errorCategory', 'changesetErrorInfo_errorCategory' - The category of the error.
--
-- -   @VALIDATION@ – The inputs to this request are invalid.
--
-- -   @SERVICE_QUOTA_EXCEEDED@ – Service quotas have been exceeded. Please
--     contact AWS support to increase quotas.
--
-- -   @ACCESS_DENIED@ – Missing required permission to perform this
--     request.
--
-- -   @RESOURCE_NOT_FOUND@ – One or more inputs to this request were not
--     found.
--
-- -   @THROTTLING@ – The system temporarily lacks sufficient resources to
--     process the request.
--
-- -   @INTERNAL_SERVICE_EXCEPTION@ – An internal service error has
--     occurred.
--
-- -   @CANCELLED@ – Cancelled.
--
-- -   @USER_RECOVERABLE@ – A user recoverable error has occurred.
--
-- 'errorMessage', 'changesetErrorInfo_errorMessage' - The text of the error message.
newChangesetErrorInfo ::
  ChangesetErrorInfo
newChangesetErrorInfo =
  ChangesetErrorInfo'
    { errorCategory =
        Prelude.Nothing,
      errorMessage = Prelude.Nothing
    }

-- | The category of the error.
--
-- -   @VALIDATION@ – The inputs to this request are invalid.
--
-- -   @SERVICE_QUOTA_EXCEEDED@ – Service quotas have been exceeded. Please
--     contact AWS support to increase quotas.
--
-- -   @ACCESS_DENIED@ – Missing required permission to perform this
--     request.
--
-- -   @RESOURCE_NOT_FOUND@ – One or more inputs to this request were not
--     found.
--
-- -   @THROTTLING@ – The system temporarily lacks sufficient resources to
--     process the request.
--
-- -   @INTERNAL_SERVICE_EXCEPTION@ – An internal service error has
--     occurred.
--
-- -   @CANCELLED@ – Cancelled.
--
-- -   @USER_RECOVERABLE@ – A user recoverable error has occurred.
changesetErrorInfo_errorCategory :: Lens.Lens' ChangesetErrorInfo (Prelude.Maybe ErrorCategory)
changesetErrorInfo_errorCategory = Lens.lens (\ChangesetErrorInfo' {errorCategory} -> errorCategory) (\s@ChangesetErrorInfo' {} a -> s {errorCategory = a} :: ChangesetErrorInfo)

-- | The text of the error message.
changesetErrorInfo_errorMessage :: Lens.Lens' ChangesetErrorInfo (Prelude.Maybe Prelude.Text)
changesetErrorInfo_errorMessage = Lens.lens (\ChangesetErrorInfo' {errorMessage} -> errorMessage) (\s@ChangesetErrorInfo' {} a -> s {errorMessage = a} :: ChangesetErrorInfo)

instance Data.FromJSON ChangesetErrorInfo where
  parseJSON =
    Data.withObject
      "ChangesetErrorInfo"
      ( \x ->
          ChangesetErrorInfo'
            Prelude.<$> (x Data..:? "errorCategory")
            Prelude.<*> (x Data..:? "errorMessage")
      )

instance Prelude.Hashable ChangesetErrorInfo where
  hashWithSalt _salt ChangesetErrorInfo' {..} =
    _salt `Prelude.hashWithSalt` errorCategory
      `Prelude.hashWithSalt` errorMessage

instance Prelude.NFData ChangesetErrorInfo where
  rnf ChangesetErrorInfo' {..} =
    Prelude.rnf errorCategory
      `Prelude.seq` Prelude.rnf errorMessage
