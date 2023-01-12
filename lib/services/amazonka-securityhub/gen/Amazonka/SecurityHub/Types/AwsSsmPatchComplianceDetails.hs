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
-- Module      : Amazonka.SecurityHub.Types.AwsSsmPatchComplianceDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsSsmPatchComplianceDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsSsmPatch

-- | Provides information about the state of a patch on an instance based on
-- the patch baseline that was used to patch the instance.
--
-- /See:/ 'newAwsSsmPatchComplianceDetails' smart constructor.
data AwsSsmPatchComplianceDetails = AwsSsmPatchComplianceDetails'
  { -- | Information about the status of a patch.
    patch :: Prelude.Maybe AwsSsmPatch
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsSsmPatchComplianceDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'patch', 'awsSsmPatchComplianceDetails_patch' - Information about the status of a patch.
newAwsSsmPatchComplianceDetails ::
  AwsSsmPatchComplianceDetails
newAwsSsmPatchComplianceDetails =
  AwsSsmPatchComplianceDetails'
    { patch =
        Prelude.Nothing
    }

-- | Information about the status of a patch.
awsSsmPatchComplianceDetails_patch :: Lens.Lens' AwsSsmPatchComplianceDetails (Prelude.Maybe AwsSsmPatch)
awsSsmPatchComplianceDetails_patch = Lens.lens (\AwsSsmPatchComplianceDetails' {patch} -> patch) (\s@AwsSsmPatchComplianceDetails' {} a -> s {patch = a} :: AwsSsmPatchComplianceDetails)

instance Data.FromJSON AwsSsmPatchComplianceDetails where
  parseJSON =
    Data.withObject
      "AwsSsmPatchComplianceDetails"
      ( \x ->
          AwsSsmPatchComplianceDetails'
            Prelude.<$> (x Data..:? "Patch")
      )

instance
  Prelude.Hashable
    AwsSsmPatchComplianceDetails
  where
  hashWithSalt _salt AwsSsmPatchComplianceDetails' {..} =
    _salt `Prelude.hashWithSalt` patch

instance Prelude.NFData AwsSsmPatchComplianceDetails where
  rnf AwsSsmPatchComplianceDetails' {..} =
    Prelude.rnf patch

instance Data.ToJSON AwsSsmPatchComplianceDetails where
  toJSON AwsSsmPatchComplianceDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [("Patch" Data..=) Prelude.<$> patch]
      )
