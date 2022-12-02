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
-- Module      : Amazonka.Inspector2.Types.InspectorScoreDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector2.Types.InspectorScoreDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Inspector2.Types.CvssScoreDetails
import qualified Amazonka.Prelude as Prelude

-- | Information about the Amazon Inspector score given to a finding.
--
-- /See:/ 'newInspectorScoreDetails' smart constructor.
data InspectorScoreDetails = InspectorScoreDetails'
  { -- | An object that contains details about the CVSS score given to a finding.
    adjustedCvss :: Prelude.Maybe CvssScoreDetails
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InspectorScoreDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'adjustedCvss', 'inspectorScoreDetails_adjustedCvss' - An object that contains details about the CVSS score given to a finding.
newInspectorScoreDetails ::
  InspectorScoreDetails
newInspectorScoreDetails =
  InspectorScoreDetails'
    { adjustedCvss =
        Prelude.Nothing
    }

-- | An object that contains details about the CVSS score given to a finding.
inspectorScoreDetails_adjustedCvss :: Lens.Lens' InspectorScoreDetails (Prelude.Maybe CvssScoreDetails)
inspectorScoreDetails_adjustedCvss = Lens.lens (\InspectorScoreDetails' {adjustedCvss} -> adjustedCvss) (\s@InspectorScoreDetails' {} a -> s {adjustedCvss = a} :: InspectorScoreDetails)

instance Data.FromJSON InspectorScoreDetails where
  parseJSON =
    Data.withObject
      "InspectorScoreDetails"
      ( \x ->
          InspectorScoreDetails'
            Prelude.<$> (x Data..:? "adjustedCvss")
      )

instance Prelude.Hashable InspectorScoreDetails where
  hashWithSalt _salt InspectorScoreDetails' {..} =
    _salt `Prelude.hashWithSalt` adjustedCvss

instance Prelude.NFData InspectorScoreDetails where
  rnf InspectorScoreDetails' {..} =
    Prelude.rnf adjustedCvss
