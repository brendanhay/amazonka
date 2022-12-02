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
-- Module      : Amazonka.AppMesh.Types.Backend
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppMesh.Types.Backend where

import Amazonka.AppMesh.Types.VirtualServiceBackend
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object that represents the backends that a virtual node is expected
-- to send outbound traffic to.
--
-- /See:/ 'newBackend' smart constructor.
data Backend = Backend'
  { -- | Specifies a virtual service to use as a backend.
    virtualService :: Prelude.Maybe VirtualServiceBackend
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Backend' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'virtualService', 'backend_virtualService' - Specifies a virtual service to use as a backend.
newBackend ::
  Backend
newBackend =
  Backend' {virtualService = Prelude.Nothing}

-- | Specifies a virtual service to use as a backend.
backend_virtualService :: Lens.Lens' Backend (Prelude.Maybe VirtualServiceBackend)
backend_virtualService = Lens.lens (\Backend' {virtualService} -> virtualService) (\s@Backend' {} a -> s {virtualService = a} :: Backend)

instance Data.FromJSON Backend where
  parseJSON =
    Data.withObject
      "Backend"
      ( \x ->
          Backend' Prelude.<$> (x Data..:? "virtualService")
      )

instance Prelude.Hashable Backend where
  hashWithSalt _salt Backend' {..} =
    _salt `Prelude.hashWithSalt` virtualService

instance Prelude.NFData Backend where
  rnf Backend' {..} = Prelude.rnf virtualService

instance Data.ToJSON Backend where
  toJSON Backend' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("virtualService" Data..=)
              Prelude.<$> virtualService
          ]
      )
