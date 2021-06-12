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
-- Module      : Network.AWS.Lightsail.Types.AddOnRequest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.AddOnRequest where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types.AddOnType
import Network.AWS.Lightsail.Types.AutoSnapshotAddOnRequest

-- | Describes a request to enable, modify, or disable an add-on for an
-- Amazon Lightsail resource.
--
-- An additional cost may be associated with enabling add-ons. For more
-- information, see the
-- <https://aws.amazon.com/lightsail/pricing/ Lightsail pricing page>.
--
-- /See:/ 'newAddOnRequest' smart constructor.
data AddOnRequest = AddOnRequest'
  { -- | An object that represents additional parameters when enabling or
    -- modifying the automatic snapshot add-on.
    autoSnapshotAddOnRequest :: Core.Maybe AutoSnapshotAddOnRequest,
    -- | The add-on type.
    addOnType :: AddOnType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AddOnRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'autoSnapshotAddOnRequest', 'addOnRequest_autoSnapshotAddOnRequest' - An object that represents additional parameters when enabling or
-- modifying the automatic snapshot add-on.
--
-- 'addOnType', 'addOnRequest_addOnType' - The add-on type.
newAddOnRequest ::
  -- | 'addOnType'
  AddOnType ->
  AddOnRequest
newAddOnRequest pAddOnType_ =
  AddOnRequest'
    { autoSnapshotAddOnRequest =
        Core.Nothing,
      addOnType = pAddOnType_
    }

-- | An object that represents additional parameters when enabling or
-- modifying the automatic snapshot add-on.
addOnRequest_autoSnapshotAddOnRequest :: Lens.Lens' AddOnRequest (Core.Maybe AutoSnapshotAddOnRequest)
addOnRequest_autoSnapshotAddOnRequest = Lens.lens (\AddOnRequest' {autoSnapshotAddOnRequest} -> autoSnapshotAddOnRequest) (\s@AddOnRequest' {} a -> s {autoSnapshotAddOnRequest = a} :: AddOnRequest)

-- | The add-on type.
addOnRequest_addOnType :: Lens.Lens' AddOnRequest AddOnType
addOnRequest_addOnType = Lens.lens (\AddOnRequest' {addOnType} -> addOnType) (\s@AddOnRequest' {} a -> s {addOnType = a} :: AddOnRequest)

instance Core.Hashable AddOnRequest

instance Core.NFData AddOnRequest

instance Core.ToJSON AddOnRequest where
  toJSON AddOnRequest' {..} =
    Core.object
      ( Core.catMaybes
          [ ("autoSnapshotAddOnRequest" Core..=)
              Core.<$> autoSnapshotAddOnRequest,
            Core.Just ("addOnType" Core..= addOnType)
          ]
      )
