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
-- Module      : Amazonka.GuardDuty.Types.SecurityContext
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.SecurityContext where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Container security context.
--
-- /See:/ 'newSecurityContext' smart constructor.
data SecurityContext = SecurityContext'
  { -- | Whether the container is privileged.
    privileged :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SecurityContext' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'privileged', 'securityContext_privileged' - Whether the container is privileged.
newSecurityContext ::
  SecurityContext
newSecurityContext =
  SecurityContext' {privileged = Prelude.Nothing}

-- | Whether the container is privileged.
securityContext_privileged :: Lens.Lens' SecurityContext (Prelude.Maybe Prelude.Bool)
securityContext_privileged = Lens.lens (\SecurityContext' {privileged} -> privileged) (\s@SecurityContext' {} a -> s {privileged = a} :: SecurityContext)

instance Core.FromJSON SecurityContext where
  parseJSON =
    Core.withObject
      "SecurityContext"
      ( \x ->
          SecurityContext'
            Prelude.<$> (x Core..:? "privileged")
      )

instance Prelude.Hashable SecurityContext where
  hashWithSalt _salt SecurityContext' {..} =
    _salt `Prelude.hashWithSalt` privileged

instance Prelude.NFData SecurityContext where
  rnf SecurityContext' {..} = Prelude.rnf privileged
