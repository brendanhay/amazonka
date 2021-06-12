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
-- Module      : Network.AWS.Pinpoint.Types.UpdateAttributesRequest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.UpdateAttributesRequest where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Specifies one or more attributes to remove from all the endpoints that
-- are associated with an application.
--
-- /See:/ 'newUpdateAttributesRequest' smart constructor.
data UpdateAttributesRequest = UpdateAttributesRequest'
  { -- | An array of the attributes to remove from all the endpoints that are
    -- associated with the application. The array can specify the complete,
    -- exact name of each attribute to remove or it can specify a glob pattern
    -- that an attribute name must match in order for the attribute to be
    -- removed.
    blacklist :: Core.Maybe [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateAttributesRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'blacklist', 'updateAttributesRequest_blacklist' - An array of the attributes to remove from all the endpoints that are
-- associated with the application. The array can specify the complete,
-- exact name of each attribute to remove or it can specify a glob pattern
-- that an attribute name must match in order for the attribute to be
-- removed.
newUpdateAttributesRequest ::
  UpdateAttributesRequest
newUpdateAttributesRequest =
  UpdateAttributesRequest' {blacklist = Core.Nothing}

-- | An array of the attributes to remove from all the endpoints that are
-- associated with the application. The array can specify the complete,
-- exact name of each attribute to remove or it can specify a glob pattern
-- that an attribute name must match in order for the attribute to be
-- removed.
updateAttributesRequest_blacklist :: Lens.Lens' UpdateAttributesRequest (Core.Maybe [Core.Text])
updateAttributesRequest_blacklist = Lens.lens (\UpdateAttributesRequest' {blacklist} -> blacklist) (\s@UpdateAttributesRequest' {} a -> s {blacklist = a} :: UpdateAttributesRequest) Core.. Lens.mapping Lens._Coerce

instance Core.Hashable UpdateAttributesRequest

instance Core.NFData UpdateAttributesRequest

instance Core.ToJSON UpdateAttributesRequest where
  toJSON UpdateAttributesRequest' {..} =
    Core.object
      ( Core.catMaybes
          [("Blacklist" Core..=) Core.<$> blacklist]
      )
