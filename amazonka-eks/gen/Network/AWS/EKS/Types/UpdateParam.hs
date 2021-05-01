{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.EKS.Types.UpdateParam
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EKS.Types.UpdateParam where

import Network.AWS.EKS.Types.UpdateParamType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | An object representing the details of an update request.
--
-- /See:/ 'newUpdateParam' smart constructor.
data UpdateParam = UpdateParam'
  { -- | The value of the keys submitted as part of an update request.
    value :: Prelude.Maybe Prelude.Text,
    -- | The keys associated with an update request.
    type' :: Prelude.Maybe UpdateParamType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateParam' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'value', 'updateParam_value' - The value of the keys submitted as part of an update request.
--
-- 'type'', 'updateParam_type' - The keys associated with an update request.
newUpdateParam ::
  UpdateParam
newUpdateParam =
  UpdateParam'
    { value = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The value of the keys submitted as part of an update request.
updateParam_value :: Lens.Lens' UpdateParam (Prelude.Maybe Prelude.Text)
updateParam_value = Lens.lens (\UpdateParam' {value} -> value) (\s@UpdateParam' {} a -> s {value = a} :: UpdateParam)

-- | The keys associated with an update request.
updateParam_type :: Lens.Lens' UpdateParam (Prelude.Maybe UpdateParamType)
updateParam_type = Lens.lens (\UpdateParam' {type'} -> type') (\s@UpdateParam' {} a -> s {type' = a} :: UpdateParam)

instance Prelude.FromJSON UpdateParam where
  parseJSON =
    Prelude.withObject
      "UpdateParam"
      ( \x ->
          UpdateParam'
            Prelude.<$> (x Prelude..:? "value")
            Prelude.<*> (x Prelude..:? "type")
      )

instance Prelude.Hashable UpdateParam

instance Prelude.NFData UpdateParam
