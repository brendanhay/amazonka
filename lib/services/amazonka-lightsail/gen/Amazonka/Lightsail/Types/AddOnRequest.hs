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
-- Module      : Amazonka.Lightsail.Types.AddOnRequest
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lightsail.Types.AddOnRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lightsail.Types.AddOnType
import Amazonka.Lightsail.Types.AutoSnapshotAddOnRequest
import qualified Amazonka.Prelude as Prelude

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
    autoSnapshotAddOnRequest :: Prelude.Maybe AutoSnapshotAddOnRequest,
    -- | The add-on type.
    addOnType :: AddOnType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
        Prelude.Nothing,
      addOnType = pAddOnType_
    }

-- | An object that represents additional parameters when enabling or
-- modifying the automatic snapshot add-on.
addOnRequest_autoSnapshotAddOnRequest :: Lens.Lens' AddOnRequest (Prelude.Maybe AutoSnapshotAddOnRequest)
addOnRequest_autoSnapshotAddOnRequest = Lens.lens (\AddOnRequest' {autoSnapshotAddOnRequest} -> autoSnapshotAddOnRequest) (\s@AddOnRequest' {} a -> s {autoSnapshotAddOnRequest = a} :: AddOnRequest)

-- | The add-on type.
addOnRequest_addOnType :: Lens.Lens' AddOnRequest AddOnType
addOnRequest_addOnType = Lens.lens (\AddOnRequest' {addOnType} -> addOnType) (\s@AddOnRequest' {} a -> s {addOnType = a} :: AddOnRequest)

instance Prelude.Hashable AddOnRequest where
  hashWithSalt _salt AddOnRequest' {..} =
    _salt
      `Prelude.hashWithSalt` autoSnapshotAddOnRequest
      `Prelude.hashWithSalt` addOnType

instance Prelude.NFData AddOnRequest where
  rnf AddOnRequest' {..} =
    Prelude.rnf autoSnapshotAddOnRequest `Prelude.seq`
      Prelude.rnf addOnType

instance Data.ToJSON AddOnRequest where
  toJSON AddOnRequest' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("autoSnapshotAddOnRequest" Data..=)
              Prelude.<$> autoSnapshotAddOnRequest,
            Prelude.Just ("addOnType" Data..= addOnType)
          ]
      )
