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
-- Module      : Amazonka.Proton.Types.AccountSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Proton.Types.AccountSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The AWS Proton pipeline service role data.
--
-- /See:/ 'newAccountSettings' smart constructor.
data AccountSettings = AccountSettings'
  { -- | The Amazon Resource Name (ARN) of the AWS Proton pipeline service role.
    pipelineServiceRoleArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AccountSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pipelineServiceRoleArn', 'accountSettings_pipelineServiceRoleArn' - The Amazon Resource Name (ARN) of the AWS Proton pipeline service role.
newAccountSettings ::
  AccountSettings
newAccountSettings =
  AccountSettings'
    { pipelineServiceRoleArn =
        Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the AWS Proton pipeline service role.
accountSettings_pipelineServiceRoleArn :: Lens.Lens' AccountSettings (Prelude.Maybe Prelude.Text)
accountSettings_pipelineServiceRoleArn = Lens.lens (\AccountSettings' {pipelineServiceRoleArn} -> pipelineServiceRoleArn) (\s@AccountSettings' {} a -> s {pipelineServiceRoleArn = a} :: AccountSettings)

instance Core.FromJSON AccountSettings where
  parseJSON =
    Core.withObject
      "AccountSettings"
      ( \x ->
          AccountSettings'
            Prelude.<$> (x Core..:? "pipelineServiceRoleArn")
      )

instance Prelude.Hashable AccountSettings where
  hashWithSalt _salt AccountSettings' {..} =
    _salt `Prelude.hashWithSalt` pipelineServiceRoleArn

instance Prelude.NFData AccountSettings where
  rnf AccountSettings' {..} =
    Prelude.rnf pipelineServiceRoleArn
