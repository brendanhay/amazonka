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
-- Module      : Amazonka.SecurityHub.Types.UnprocessedStandardsControlAssociationUpdate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.UnprocessedStandardsControlAssociationUpdate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.StandardsControlAssociationUpdate
import Amazonka.SecurityHub.Types.UnprocessedErrorCode

-- | Provides details about which control\'s enablement status could not be
-- updated in a specified standard when calling the
-- <https://docs.aws.amazon.com/securityhub/1.0/APIReference/API_BatchUpdateStandardsControlAssociations.html BatchUpdateStandardsControlAssociations>
-- API. This parameter also provides details about why the request was
-- unprocessed.
--
-- /See:/ 'newUnprocessedStandardsControlAssociationUpdate' smart constructor.
data UnprocessedStandardsControlAssociationUpdate = UnprocessedStandardsControlAssociationUpdate'
  { -- | The reason why a control\'s enablement status in the specified standard
    -- couldn\'t be updated.
    errorReason :: Prelude.Maybe Prelude.Text,
    -- | An array of control and standard associations for which an update failed
    -- when calling
    -- <https://docs.aws.amazon.com/securityhub/1.0/APIReference/API_BatchUpdateStandardsControlAssociations.html BatchUpdateStandardsControlAssociations>.
    standardsControlAssociationUpdate :: StandardsControlAssociationUpdate,
    -- | The error code for the unprocessed update of the control\'s enablement
    -- status in the specified standard.
    errorCode :: UnprocessedErrorCode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UnprocessedStandardsControlAssociationUpdate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errorReason', 'unprocessedStandardsControlAssociationUpdate_errorReason' - The reason why a control\'s enablement status in the specified standard
-- couldn\'t be updated.
--
-- 'standardsControlAssociationUpdate', 'unprocessedStandardsControlAssociationUpdate_standardsControlAssociationUpdate' - An array of control and standard associations for which an update failed
-- when calling
-- <https://docs.aws.amazon.com/securityhub/1.0/APIReference/API_BatchUpdateStandardsControlAssociations.html BatchUpdateStandardsControlAssociations>.
--
-- 'errorCode', 'unprocessedStandardsControlAssociationUpdate_errorCode' - The error code for the unprocessed update of the control\'s enablement
-- status in the specified standard.
newUnprocessedStandardsControlAssociationUpdate ::
  -- | 'standardsControlAssociationUpdate'
  StandardsControlAssociationUpdate ->
  -- | 'errorCode'
  UnprocessedErrorCode ->
  UnprocessedStandardsControlAssociationUpdate
newUnprocessedStandardsControlAssociationUpdate
  pStandardsControlAssociationUpdate_
  pErrorCode_ =
    UnprocessedStandardsControlAssociationUpdate'
      { errorReason =
          Prelude.Nothing,
        standardsControlAssociationUpdate =
          pStandardsControlAssociationUpdate_,
        errorCode = pErrorCode_
      }

-- | The reason why a control\'s enablement status in the specified standard
-- couldn\'t be updated.
unprocessedStandardsControlAssociationUpdate_errorReason :: Lens.Lens' UnprocessedStandardsControlAssociationUpdate (Prelude.Maybe Prelude.Text)
unprocessedStandardsControlAssociationUpdate_errorReason = Lens.lens (\UnprocessedStandardsControlAssociationUpdate' {errorReason} -> errorReason) (\s@UnprocessedStandardsControlAssociationUpdate' {} a -> s {errorReason = a} :: UnprocessedStandardsControlAssociationUpdate)

-- | An array of control and standard associations for which an update failed
-- when calling
-- <https://docs.aws.amazon.com/securityhub/1.0/APIReference/API_BatchUpdateStandardsControlAssociations.html BatchUpdateStandardsControlAssociations>.
unprocessedStandardsControlAssociationUpdate_standardsControlAssociationUpdate :: Lens.Lens' UnprocessedStandardsControlAssociationUpdate StandardsControlAssociationUpdate
unprocessedStandardsControlAssociationUpdate_standardsControlAssociationUpdate = Lens.lens (\UnprocessedStandardsControlAssociationUpdate' {standardsControlAssociationUpdate} -> standardsControlAssociationUpdate) (\s@UnprocessedStandardsControlAssociationUpdate' {} a -> s {standardsControlAssociationUpdate = a} :: UnprocessedStandardsControlAssociationUpdate)

-- | The error code for the unprocessed update of the control\'s enablement
-- status in the specified standard.
unprocessedStandardsControlAssociationUpdate_errorCode :: Lens.Lens' UnprocessedStandardsControlAssociationUpdate UnprocessedErrorCode
unprocessedStandardsControlAssociationUpdate_errorCode = Lens.lens (\UnprocessedStandardsControlAssociationUpdate' {errorCode} -> errorCode) (\s@UnprocessedStandardsControlAssociationUpdate' {} a -> s {errorCode = a} :: UnprocessedStandardsControlAssociationUpdate)

instance
  Data.FromJSON
    UnprocessedStandardsControlAssociationUpdate
  where
  parseJSON =
    Data.withObject
      "UnprocessedStandardsControlAssociationUpdate"
      ( \x ->
          UnprocessedStandardsControlAssociationUpdate'
            Prelude.<$> (x Data..:? "ErrorReason")
            Prelude.<*> (x Data..: "StandardsControlAssociationUpdate")
            Prelude.<*> (x Data..: "ErrorCode")
      )

instance
  Prelude.Hashable
    UnprocessedStandardsControlAssociationUpdate
  where
  hashWithSalt
    _salt
    UnprocessedStandardsControlAssociationUpdate' {..} =
      _salt
        `Prelude.hashWithSalt` errorReason
        `Prelude.hashWithSalt` standardsControlAssociationUpdate
        `Prelude.hashWithSalt` errorCode

instance
  Prelude.NFData
    UnprocessedStandardsControlAssociationUpdate
  where
  rnf UnprocessedStandardsControlAssociationUpdate' {..} =
    Prelude.rnf errorReason
      `Prelude.seq` Prelude.rnf standardsControlAssociationUpdate
      `Prelude.seq` Prelude.rnf errorCode
