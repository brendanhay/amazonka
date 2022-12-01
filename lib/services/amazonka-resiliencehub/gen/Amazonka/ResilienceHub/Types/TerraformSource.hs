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
-- Module      : Amazonka.ResilienceHub.Types.TerraformSource
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ResilienceHub.Types.TerraformSource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The Terraform s3 state file you need to import.
--
-- /See:/ 'newTerraformSource' smart constructor.
data TerraformSource = TerraformSource'
  { -- | The Terraform s3 state file you need to import.
    s3StateFileUrl :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TerraformSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3StateFileUrl', 'terraformSource_s3StateFileUrl' - The Terraform s3 state file you need to import.
newTerraformSource ::
  -- | 's3StateFileUrl'
  Prelude.Text ->
  TerraformSource
newTerraformSource pS3StateFileUrl_ =
  TerraformSource' {s3StateFileUrl = pS3StateFileUrl_}

-- | The Terraform s3 state file you need to import.
terraformSource_s3StateFileUrl :: Lens.Lens' TerraformSource Prelude.Text
terraformSource_s3StateFileUrl = Lens.lens (\TerraformSource' {s3StateFileUrl} -> s3StateFileUrl) (\s@TerraformSource' {} a -> s {s3StateFileUrl = a} :: TerraformSource)

instance Core.FromJSON TerraformSource where
  parseJSON =
    Core.withObject
      "TerraformSource"
      ( \x ->
          TerraformSource'
            Prelude.<$> (x Core..: "s3StateFileUrl")
      )

instance Prelude.Hashable TerraformSource where
  hashWithSalt _salt TerraformSource' {..} =
    _salt `Prelude.hashWithSalt` s3StateFileUrl

instance Prelude.NFData TerraformSource where
  rnf TerraformSource' {..} = Prelude.rnf s3StateFileUrl

instance Core.ToJSON TerraformSource where
  toJSON TerraformSource' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("s3StateFileUrl" Core..= s3StateFileUrl)
          ]
      )
