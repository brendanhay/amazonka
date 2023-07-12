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
-- Module      : Amazonka.EMR.Types.OSRelease
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EMR.Types.OSRelease where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The Amazon Linux release specified for a cluster in the RunJobFlow
-- request.
--
-- /See:/ 'newOSRelease' smart constructor.
data OSRelease = OSRelease'
  { -- | The Amazon Linux release specified for a cluster in the RunJobFlow
    -- request. The format is as shown in
    -- <https://docs.aws.amazon.com/AL2/latest/relnotes/relnotes-20220218.html Amazon Linux 2 Release Notes>
    -- . For example, 2.0.20220218.1.
    label :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OSRelease' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'label', 'oSRelease_label' - The Amazon Linux release specified for a cluster in the RunJobFlow
-- request. The format is as shown in
-- <https://docs.aws.amazon.com/AL2/latest/relnotes/relnotes-20220218.html Amazon Linux 2 Release Notes>
-- . For example, 2.0.20220218.1.
newOSRelease ::
  OSRelease
newOSRelease = OSRelease' {label = Prelude.Nothing}

-- | The Amazon Linux release specified for a cluster in the RunJobFlow
-- request. The format is as shown in
-- <https://docs.aws.amazon.com/AL2/latest/relnotes/relnotes-20220218.html Amazon Linux 2 Release Notes>
-- . For example, 2.0.20220218.1.
oSRelease_label :: Lens.Lens' OSRelease (Prelude.Maybe Prelude.Text)
oSRelease_label = Lens.lens (\OSRelease' {label} -> label) (\s@OSRelease' {} a -> s {label = a} :: OSRelease)

instance Data.FromJSON OSRelease where
  parseJSON =
    Data.withObject
      "OSRelease"
      (\x -> OSRelease' Prelude.<$> (x Data..:? "Label"))

instance Prelude.Hashable OSRelease where
  hashWithSalt _salt OSRelease' {..} =
    _salt `Prelude.hashWithSalt` label

instance Prelude.NFData OSRelease where
  rnf OSRelease' {..} = Prelude.rnf label
