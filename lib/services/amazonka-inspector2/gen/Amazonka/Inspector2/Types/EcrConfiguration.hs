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
-- Module      : Amazonka.Inspector2.Types.EcrConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector2.Types.EcrConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Inspector2.Types.EcrRescanDuration
import qualified Amazonka.Prelude as Prelude

-- | Details about the ECR automated re-scan duration setting for your
-- environment.
--
-- /See:/ 'newEcrConfiguration' smart constructor.
data EcrConfiguration = EcrConfiguration'
  { -- | The ECR automated re-scan duration defines how long an ECR image will be
    -- actively scanned by Amazon Inspector. When the number of days since an
    -- image was last pushed exceeds the automated re-scan duration the
    -- monitoring state of that image becomes @inactive@ and all associated
    -- findings are scheduled for closure.
    rescanDuration :: EcrRescanDuration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EcrConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'rescanDuration', 'ecrConfiguration_rescanDuration' - The ECR automated re-scan duration defines how long an ECR image will be
-- actively scanned by Amazon Inspector. When the number of days since an
-- image was last pushed exceeds the automated re-scan duration the
-- monitoring state of that image becomes @inactive@ and all associated
-- findings are scheduled for closure.
newEcrConfiguration ::
  -- | 'rescanDuration'
  EcrRescanDuration ->
  EcrConfiguration
newEcrConfiguration pRescanDuration_ =
  EcrConfiguration'
    { rescanDuration =
        pRescanDuration_
    }

-- | The ECR automated re-scan duration defines how long an ECR image will be
-- actively scanned by Amazon Inspector. When the number of days since an
-- image was last pushed exceeds the automated re-scan duration the
-- monitoring state of that image becomes @inactive@ and all associated
-- findings are scheduled for closure.
ecrConfiguration_rescanDuration :: Lens.Lens' EcrConfiguration EcrRescanDuration
ecrConfiguration_rescanDuration = Lens.lens (\EcrConfiguration' {rescanDuration} -> rescanDuration) (\s@EcrConfiguration' {} a -> s {rescanDuration = a} :: EcrConfiguration)

instance Prelude.Hashable EcrConfiguration where
  hashWithSalt _salt EcrConfiguration' {..} =
    _salt `Prelude.hashWithSalt` rescanDuration

instance Prelude.NFData EcrConfiguration where
  rnf EcrConfiguration' {..} =
    Prelude.rnf rescanDuration

instance Data.ToJSON EcrConfiguration where
  toJSON EcrConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("rescanDuration" Data..= rescanDuration)
          ]
      )
