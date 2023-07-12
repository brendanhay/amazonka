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
-- Module      : Amazonka.SecurityHub.Types.AwsSsmPatch
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsSsmPatch where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsSsmComplianceSummary

-- | Provides details about the compliance for a patch.
--
-- /See:/ 'newAwsSsmPatch' smart constructor.
data AwsSsmPatch = AwsSsmPatch'
  { -- | The compliance status details for the patch.
    complianceSummary :: Prelude.Maybe AwsSsmComplianceSummary
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsSsmPatch' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'complianceSummary', 'awsSsmPatch_complianceSummary' - The compliance status details for the patch.
newAwsSsmPatch ::
  AwsSsmPatch
newAwsSsmPatch =
  AwsSsmPatch' {complianceSummary = Prelude.Nothing}

-- | The compliance status details for the patch.
awsSsmPatch_complianceSummary :: Lens.Lens' AwsSsmPatch (Prelude.Maybe AwsSsmComplianceSummary)
awsSsmPatch_complianceSummary = Lens.lens (\AwsSsmPatch' {complianceSummary} -> complianceSummary) (\s@AwsSsmPatch' {} a -> s {complianceSummary = a} :: AwsSsmPatch)

instance Data.FromJSON AwsSsmPatch where
  parseJSON =
    Data.withObject
      "AwsSsmPatch"
      ( \x ->
          AwsSsmPatch'
            Prelude.<$> (x Data..:? "ComplianceSummary")
      )

instance Prelude.Hashable AwsSsmPatch where
  hashWithSalt _salt AwsSsmPatch' {..} =
    _salt `Prelude.hashWithSalt` complianceSummary

instance Prelude.NFData AwsSsmPatch where
  rnf AwsSsmPatch' {..} = Prelude.rnf complianceSummary

instance Data.ToJSON AwsSsmPatch where
  toJSON AwsSsmPatch' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ComplianceSummary" Data..=)
              Prelude.<$> complianceSummary
          ]
      )
