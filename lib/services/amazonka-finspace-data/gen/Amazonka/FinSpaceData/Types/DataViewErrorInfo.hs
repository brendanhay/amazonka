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
-- Module      : Amazonka.FinSpaceData.Types.DataViewErrorInfo
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FinSpaceData.Types.DataViewErrorInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FinSpaceData.Types.ErrorCategory
import qualified Amazonka.Prelude as Prelude

-- | The structure with error messages.
--
-- /See:/ 'newDataViewErrorInfo' smart constructor.
data DataViewErrorInfo = DataViewErrorInfo'
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
-- Create a value of 'DataViewErrorInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errorCategory', 'dataViewErrorInfo_errorCategory' - The category of the error.
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
-- 'errorMessage', 'dataViewErrorInfo_errorMessage' - The text of the error message.
newDataViewErrorInfo ::
  DataViewErrorInfo
newDataViewErrorInfo =
  DataViewErrorInfo'
    { errorCategory = Prelude.Nothing,
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
dataViewErrorInfo_errorCategory :: Lens.Lens' DataViewErrorInfo (Prelude.Maybe ErrorCategory)
dataViewErrorInfo_errorCategory = Lens.lens (\DataViewErrorInfo' {errorCategory} -> errorCategory) (\s@DataViewErrorInfo' {} a -> s {errorCategory = a} :: DataViewErrorInfo)

-- | The text of the error message.
dataViewErrorInfo_errorMessage :: Lens.Lens' DataViewErrorInfo (Prelude.Maybe Prelude.Text)
dataViewErrorInfo_errorMessage = Lens.lens (\DataViewErrorInfo' {errorMessage} -> errorMessage) (\s@DataViewErrorInfo' {} a -> s {errorMessage = a} :: DataViewErrorInfo)

instance Data.FromJSON DataViewErrorInfo where
  parseJSON =
    Data.withObject
      "DataViewErrorInfo"
      ( \x ->
          DataViewErrorInfo'
            Prelude.<$> (x Data..:? "errorCategory")
            Prelude.<*> (x Data..:? "errorMessage")
      )

instance Prelude.Hashable DataViewErrorInfo where
  hashWithSalt _salt DataViewErrorInfo' {..} =
    _salt
      `Prelude.hashWithSalt` errorCategory
      `Prelude.hashWithSalt` errorMessage

instance Prelude.NFData DataViewErrorInfo where
  rnf DataViewErrorInfo' {..} =
    Prelude.rnf errorCategory `Prelude.seq`
      Prelude.rnf errorMessage
