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
-- Module      : Amazonka.AppMesh.Types.VirtualServiceStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppMesh.Types.VirtualServiceStatus where

import Amazonka.AppMesh.Types.VirtualServiceStatusCode
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | An object that represents the status of a virtual service.
--
-- /See:/ 'newVirtualServiceStatus' smart constructor.
data VirtualServiceStatus = VirtualServiceStatus'
  { -- | The current status of the virtual service.
    status :: VirtualServiceStatusCode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VirtualServiceStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'virtualServiceStatus_status' - The current status of the virtual service.
newVirtualServiceStatus ::
  -- | 'status'
  VirtualServiceStatusCode ->
  VirtualServiceStatus
newVirtualServiceStatus pStatus_ =
  VirtualServiceStatus' {status = pStatus_}

-- | The current status of the virtual service.
virtualServiceStatus_status :: Lens.Lens' VirtualServiceStatus VirtualServiceStatusCode
virtualServiceStatus_status = Lens.lens (\VirtualServiceStatus' {status} -> status) (\s@VirtualServiceStatus' {} a -> s {status = a} :: VirtualServiceStatus)

instance Core.FromJSON VirtualServiceStatus where
  parseJSON =
    Core.withObject
      "VirtualServiceStatus"
      ( \x ->
          VirtualServiceStatus'
            Prelude.<$> (x Core..: "status")
      )

instance Prelude.Hashable VirtualServiceStatus where
  hashWithSalt _salt VirtualServiceStatus' {..} =
    _salt `Prelude.hashWithSalt` status

instance Prelude.NFData VirtualServiceStatus where
  rnf VirtualServiceStatus' {..} = Prelude.rnf status
