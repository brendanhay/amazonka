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
-- Module      : Amazonka.FraudDetector.Types.ExternalEventsDetail
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FraudDetector.Types.ExternalEventsDetail where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Details for the external events data used for model version training.
--
-- /See:/ 'newExternalEventsDetail' smart constructor.
data ExternalEventsDetail = ExternalEventsDetail'
  { -- | The Amazon S3 bucket location for the data.
    dataLocation :: Prelude.Text,
    -- | The ARN of the role that provides Amazon Fraud Detector access to the
    -- data location.
    dataAccessRoleArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExternalEventsDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataLocation', 'externalEventsDetail_dataLocation' - The Amazon S3 bucket location for the data.
--
-- 'dataAccessRoleArn', 'externalEventsDetail_dataAccessRoleArn' - The ARN of the role that provides Amazon Fraud Detector access to the
-- data location.
newExternalEventsDetail ::
  -- | 'dataLocation'
  Prelude.Text ->
  -- | 'dataAccessRoleArn'
  Prelude.Text ->
  ExternalEventsDetail
newExternalEventsDetail
  pDataLocation_
  pDataAccessRoleArn_ =
    ExternalEventsDetail'
      { dataLocation =
          pDataLocation_,
        dataAccessRoleArn = pDataAccessRoleArn_
      }

-- | The Amazon S3 bucket location for the data.
externalEventsDetail_dataLocation :: Lens.Lens' ExternalEventsDetail Prelude.Text
externalEventsDetail_dataLocation = Lens.lens (\ExternalEventsDetail' {dataLocation} -> dataLocation) (\s@ExternalEventsDetail' {} a -> s {dataLocation = a} :: ExternalEventsDetail)

-- | The ARN of the role that provides Amazon Fraud Detector access to the
-- data location.
externalEventsDetail_dataAccessRoleArn :: Lens.Lens' ExternalEventsDetail Prelude.Text
externalEventsDetail_dataAccessRoleArn = Lens.lens (\ExternalEventsDetail' {dataAccessRoleArn} -> dataAccessRoleArn) (\s@ExternalEventsDetail' {} a -> s {dataAccessRoleArn = a} :: ExternalEventsDetail)

instance Data.FromJSON ExternalEventsDetail where
  parseJSON =
    Data.withObject
      "ExternalEventsDetail"
      ( \x ->
          ExternalEventsDetail'
            Prelude.<$> (x Data..: "dataLocation")
            Prelude.<*> (x Data..: "dataAccessRoleArn")
      )

instance Prelude.Hashable ExternalEventsDetail where
  hashWithSalt _salt ExternalEventsDetail' {..} =
    _salt
      `Prelude.hashWithSalt` dataLocation
      `Prelude.hashWithSalt` dataAccessRoleArn

instance Prelude.NFData ExternalEventsDetail where
  rnf ExternalEventsDetail' {..} =
    Prelude.rnf dataLocation `Prelude.seq`
      Prelude.rnf dataAccessRoleArn

instance Data.ToJSON ExternalEventsDetail where
  toJSON ExternalEventsDetail' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("dataLocation" Data..= dataLocation),
            Prelude.Just
              ("dataAccessRoleArn" Data..= dataAccessRoleArn)
          ]
      )
