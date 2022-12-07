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
-- Module      : Amazonka.IAM.Types.SimulatePolicyResponse
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IAM.Types.SimulatePolicyResponse where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IAM.Types.EvaluationResult
import qualified Amazonka.Prelude as Prelude

-- | Contains the response to a successful SimulatePrincipalPolicy or
-- SimulateCustomPolicy request.
--
-- /See:/ 'newSimulatePolicyResponse' smart constructor.
data SimulatePolicyResponse = SimulatePolicyResponse'
  { -- | The results of the simulation.
    evaluationResults :: Prelude.Maybe [EvaluationResult],
    -- | When @IsTruncated@ is @true@, this element is present and contains the
    -- value to use for the @Marker@ parameter in a subsequent pagination
    -- request.
    marker :: Prelude.Maybe Prelude.Text,
    -- | A flag that indicates whether there are more items to return. If your
    -- results were truncated, you can make a subsequent pagination request
    -- using the @Marker@ request parameter to retrieve more items. Note that
    -- IAM might return fewer than the @MaxItems@ number of results even when
    -- there are more results available. We recommend that you check
    -- @IsTruncated@ after every call to ensure that you receive all your
    -- results.
    isTruncated :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SimulatePolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'evaluationResults', 'simulatePolicyResponse_evaluationResults' - The results of the simulation.
--
-- 'marker', 'simulatePolicyResponse_marker' - When @IsTruncated@ is @true@, this element is present and contains the
-- value to use for the @Marker@ parameter in a subsequent pagination
-- request.
--
-- 'isTruncated', 'simulatePolicyResponse_isTruncated' - A flag that indicates whether there are more items to return. If your
-- results were truncated, you can make a subsequent pagination request
-- using the @Marker@ request parameter to retrieve more items. Note that
-- IAM might return fewer than the @MaxItems@ number of results even when
-- there are more results available. We recommend that you check
-- @IsTruncated@ after every call to ensure that you receive all your
-- results.
newSimulatePolicyResponse ::
  SimulatePolicyResponse
newSimulatePolicyResponse =
  SimulatePolicyResponse'
    { evaluationResults =
        Prelude.Nothing,
      marker = Prelude.Nothing,
      isTruncated = Prelude.Nothing
    }

-- | The results of the simulation.
simulatePolicyResponse_evaluationResults :: Lens.Lens' SimulatePolicyResponse (Prelude.Maybe [EvaluationResult])
simulatePolicyResponse_evaluationResults = Lens.lens (\SimulatePolicyResponse' {evaluationResults} -> evaluationResults) (\s@SimulatePolicyResponse' {} a -> s {evaluationResults = a} :: SimulatePolicyResponse) Prelude.. Lens.mapping Lens.coerced

-- | When @IsTruncated@ is @true@, this element is present and contains the
-- value to use for the @Marker@ parameter in a subsequent pagination
-- request.
simulatePolicyResponse_marker :: Lens.Lens' SimulatePolicyResponse (Prelude.Maybe Prelude.Text)
simulatePolicyResponse_marker = Lens.lens (\SimulatePolicyResponse' {marker} -> marker) (\s@SimulatePolicyResponse' {} a -> s {marker = a} :: SimulatePolicyResponse)

-- | A flag that indicates whether there are more items to return. If your
-- results were truncated, you can make a subsequent pagination request
-- using the @Marker@ request parameter to retrieve more items. Note that
-- IAM might return fewer than the @MaxItems@ number of results even when
-- there are more results available. We recommend that you check
-- @IsTruncated@ after every call to ensure that you receive all your
-- results.
simulatePolicyResponse_isTruncated :: Lens.Lens' SimulatePolicyResponse (Prelude.Maybe Prelude.Bool)
simulatePolicyResponse_isTruncated = Lens.lens (\SimulatePolicyResponse' {isTruncated} -> isTruncated) (\s@SimulatePolicyResponse' {} a -> s {isTruncated = a} :: SimulatePolicyResponse)

instance Data.FromXML SimulatePolicyResponse where
  parseXML x =
    SimulatePolicyResponse'
      Prelude.<$> ( x Data..@? "EvaluationResults"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "member")
                  )
      Prelude.<*> (x Data..@? "Marker")
      Prelude.<*> (x Data..@? "IsTruncated")

instance Prelude.Hashable SimulatePolicyResponse where
  hashWithSalt _salt SimulatePolicyResponse' {..} =
    _salt `Prelude.hashWithSalt` evaluationResults
      `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` isTruncated

instance Prelude.NFData SimulatePolicyResponse where
  rnf SimulatePolicyResponse' {..} =
    Prelude.rnf evaluationResults
      `Prelude.seq` Prelude.rnf marker
      `Prelude.seq` Prelude.rnf isTruncated
