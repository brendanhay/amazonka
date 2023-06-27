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
-- Module      : Amazonka.SecurityHub.Types.UnprocessedStandardsControlAssociation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.UnprocessedStandardsControlAssociation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.StandardsControlAssociationId
import Amazonka.SecurityHub.Types.UnprocessedErrorCode

-- | Provides details about which control\'s enablement status couldn\'t be
-- retrieved in a specified standard when calling
-- <https://docs.aws.amazon.com/securityhub/1.0/APIReference/API_BatchUpdateStandardsControlAssociations.html BatchUpdateStandardsControlAssociations>.
-- This parameter also provides details about why the request was
-- unprocessed.
--
-- /See:/ 'newUnprocessedStandardsControlAssociation' smart constructor.
data UnprocessedStandardsControlAssociation = UnprocessedStandardsControlAssociation'
  { -- | The reason why the standard and control association was unprocessed.
    errorReason :: Prelude.Maybe Prelude.Text,
    -- | An array with one or more objects that includes a security control
    -- (identified with @SecurityControlId@, @SecurityControlArn@, or a mix of
    -- both parameters) and the Amazon Resource Name (ARN) of a standard. This
    -- parameter shows the specific controls for which the enablement status
    -- couldn\'t be retrieved in specified standards when calling
    -- <https://docs.aws.amazon.com/securityhub/1.0/APIReference/API_BatchUpdateStandardsControlAssociations.html BatchUpdateStandardsControlAssociations>.
    standardsControlAssociationId :: StandardsControlAssociationId,
    -- | The error code for the unprocessed standard and control association.
    errorCode :: UnprocessedErrorCode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UnprocessedStandardsControlAssociation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errorReason', 'unprocessedStandardsControlAssociation_errorReason' - The reason why the standard and control association was unprocessed.
--
-- 'standardsControlAssociationId', 'unprocessedStandardsControlAssociation_standardsControlAssociationId' - An array with one or more objects that includes a security control
-- (identified with @SecurityControlId@, @SecurityControlArn@, or a mix of
-- both parameters) and the Amazon Resource Name (ARN) of a standard. This
-- parameter shows the specific controls for which the enablement status
-- couldn\'t be retrieved in specified standards when calling
-- <https://docs.aws.amazon.com/securityhub/1.0/APIReference/API_BatchUpdateStandardsControlAssociations.html BatchUpdateStandardsControlAssociations>.
--
-- 'errorCode', 'unprocessedStandardsControlAssociation_errorCode' - The error code for the unprocessed standard and control association.
newUnprocessedStandardsControlAssociation ::
  -- | 'standardsControlAssociationId'
  StandardsControlAssociationId ->
  -- | 'errorCode'
  UnprocessedErrorCode ->
  UnprocessedStandardsControlAssociation
newUnprocessedStandardsControlAssociation
  pStandardsControlAssociationId_
  pErrorCode_ =
    UnprocessedStandardsControlAssociation'
      { errorReason =
          Prelude.Nothing,
        standardsControlAssociationId =
          pStandardsControlAssociationId_,
        errorCode = pErrorCode_
      }

-- | The reason why the standard and control association was unprocessed.
unprocessedStandardsControlAssociation_errorReason :: Lens.Lens' UnprocessedStandardsControlAssociation (Prelude.Maybe Prelude.Text)
unprocessedStandardsControlAssociation_errorReason = Lens.lens (\UnprocessedStandardsControlAssociation' {errorReason} -> errorReason) (\s@UnprocessedStandardsControlAssociation' {} a -> s {errorReason = a} :: UnprocessedStandardsControlAssociation)

-- | An array with one or more objects that includes a security control
-- (identified with @SecurityControlId@, @SecurityControlArn@, or a mix of
-- both parameters) and the Amazon Resource Name (ARN) of a standard. This
-- parameter shows the specific controls for which the enablement status
-- couldn\'t be retrieved in specified standards when calling
-- <https://docs.aws.amazon.com/securityhub/1.0/APIReference/API_BatchUpdateStandardsControlAssociations.html BatchUpdateStandardsControlAssociations>.
unprocessedStandardsControlAssociation_standardsControlAssociationId :: Lens.Lens' UnprocessedStandardsControlAssociation StandardsControlAssociationId
unprocessedStandardsControlAssociation_standardsControlAssociationId = Lens.lens (\UnprocessedStandardsControlAssociation' {standardsControlAssociationId} -> standardsControlAssociationId) (\s@UnprocessedStandardsControlAssociation' {} a -> s {standardsControlAssociationId = a} :: UnprocessedStandardsControlAssociation)

-- | The error code for the unprocessed standard and control association.
unprocessedStandardsControlAssociation_errorCode :: Lens.Lens' UnprocessedStandardsControlAssociation UnprocessedErrorCode
unprocessedStandardsControlAssociation_errorCode = Lens.lens (\UnprocessedStandardsControlAssociation' {errorCode} -> errorCode) (\s@UnprocessedStandardsControlAssociation' {} a -> s {errorCode = a} :: UnprocessedStandardsControlAssociation)

instance
  Data.FromJSON
    UnprocessedStandardsControlAssociation
  where
  parseJSON =
    Data.withObject
      "UnprocessedStandardsControlAssociation"
      ( \x ->
          UnprocessedStandardsControlAssociation'
            Prelude.<$> (x Data..:? "ErrorReason")
            Prelude.<*> (x Data..: "StandardsControlAssociationId")
            Prelude.<*> (x Data..: "ErrorCode")
      )

instance
  Prelude.Hashable
    UnprocessedStandardsControlAssociation
  where
  hashWithSalt
    _salt
    UnprocessedStandardsControlAssociation' {..} =
      _salt
        `Prelude.hashWithSalt` errorReason
        `Prelude.hashWithSalt` standardsControlAssociationId
        `Prelude.hashWithSalt` errorCode

instance
  Prelude.NFData
    UnprocessedStandardsControlAssociation
  where
  rnf UnprocessedStandardsControlAssociation' {..} =
    Prelude.rnf errorReason
      `Prelude.seq` Prelude.rnf standardsControlAssociationId
      `Prelude.seq` Prelude.rnf errorCode
