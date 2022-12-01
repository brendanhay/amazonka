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
-- Module      : Amazonka.IoTSiteWise.Types.PortalResource
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTSiteWise.Types.PortalResource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Identifies an IoT SiteWise Monitor portal.
--
-- /See:/ 'newPortalResource' smart constructor.
data PortalResource = PortalResource'
  { -- | The ID of the portal.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PortalResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'portalResource_id' - The ID of the portal.
newPortalResource ::
  -- | 'id'
  Prelude.Text ->
  PortalResource
newPortalResource pId_ = PortalResource' {id = pId_}

-- | The ID of the portal.
portalResource_id :: Lens.Lens' PortalResource Prelude.Text
portalResource_id = Lens.lens (\PortalResource' {id} -> id) (\s@PortalResource' {} a -> s {id = a} :: PortalResource)

instance Core.FromJSON PortalResource where
  parseJSON =
    Core.withObject
      "PortalResource"
      (\x -> PortalResource' Prelude.<$> (x Core..: "id"))

instance Prelude.Hashable PortalResource where
  hashWithSalt _salt PortalResource' {..} =
    _salt `Prelude.hashWithSalt` id

instance Prelude.NFData PortalResource where
  rnf PortalResource' {..} = Prelude.rnf id

instance Core.ToJSON PortalResource where
  toJSON PortalResource' {..} =
    Core.object
      (Prelude.catMaybes [Prelude.Just ("id" Core..= id)])
