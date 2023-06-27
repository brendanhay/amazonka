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
-- Module      : Amazonka.SWF.Types.RecordMarkerDecisionAttributes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SWF.Types.RecordMarkerDecisionAttributes where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides the details of the @RecordMarker@ decision.
--
-- __Access Control__
--
-- You can use IAM policies to control this decision\'s access to Amazon
-- SWF resources as follows:
--
-- -   Use a @Resource@ element with the domain name to limit the action to
--     only specified domains.
--
-- -   Use an @Action@ element to allow or deny permission to call this
--     action.
--
-- -   You cannot use an IAM policy to constrain this action\'s parameters.
--
-- If the caller doesn\'t have sufficient permissions to invoke the action,
-- or the parameter values fall outside the specified constraints, the
-- action fails. The associated event attribute\'s @cause@ parameter is set
-- to @OPERATION_NOT_PERMITTED@. For details and example IAM policies, see
-- <https://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html Using IAM to Manage Access to Amazon SWF Workflows>
-- in the /Amazon SWF Developer Guide/.
--
-- /See:/ 'newRecordMarkerDecisionAttributes' smart constructor.
data RecordMarkerDecisionAttributes = RecordMarkerDecisionAttributes'
  { -- | The details of the marker.
    details :: Prelude.Maybe Prelude.Text,
    -- | The name of the marker.
    markerName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RecordMarkerDecisionAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'details', 'recordMarkerDecisionAttributes_details' - The details of the marker.
--
-- 'markerName', 'recordMarkerDecisionAttributes_markerName' - The name of the marker.
newRecordMarkerDecisionAttributes ::
  -- | 'markerName'
  Prelude.Text ->
  RecordMarkerDecisionAttributes
newRecordMarkerDecisionAttributes pMarkerName_ =
  RecordMarkerDecisionAttributes'
    { details =
        Prelude.Nothing,
      markerName = pMarkerName_
    }

-- | The details of the marker.
recordMarkerDecisionAttributes_details :: Lens.Lens' RecordMarkerDecisionAttributes (Prelude.Maybe Prelude.Text)
recordMarkerDecisionAttributes_details = Lens.lens (\RecordMarkerDecisionAttributes' {details} -> details) (\s@RecordMarkerDecisionAttributes' {} a -> s {details = a} :: RecordMarkerDecisionAttributes)

-- | The name of the marker.
recordMarkerDecisionAttributes_markerName :: Lens.Lens' RecordMarkerDecisionAttributes Prelude.Text
recordMarkerDecisionAttributes_markerName = Lens.lens (\RecordMarkerDecisionAttributes' {markerName} -> markerName) (\s@RecordMarkerDecisionAttributes' {} a -> s {markerName = a} :: RecordMarkerDecisionAttributes)

instance
  Prelude.Hashable
    RecordMarkerDecisionAttributes
  where
  hashWithSalt
    _salt
    RecordMarkerDecisionAttributes' {..} =
      _salt
        `Prelude.hashWithSalt` details
        `Prelude.hashWithSalt` markerName

instance
  Prelude.NFData
    RecordMarkerDecisionAttributes
  where
  rnf RecordMarkerDecisionAttributes' {..} =
    Prelude.rnf details
      `Prelude.seq` Prelude.rnf markerName

instance Data.ToJSON RecordMarkerDecisionAttributes where
  toJSON RecordMarkerDecisionAttributes' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("details" Data..=) Prelude.<$> details,
            Prelude.Just ("markerName" Data..= markerName)
          ]
      )
