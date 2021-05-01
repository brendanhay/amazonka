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
-- Module      : Network.AWS.Lightsail.Types.InstanceState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.InstanceState where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes the virtual private server (or /instance/) status.
--
-- /See:/ 'newInstanceState' smart constructor.
data InstanceState = InstanceState'
  { -- | The status code for the instance.
    code :: Prelude.Maybe Prelude.Int,
    -- | The state of the instance (e.g., @running@ or @pending@).
    name :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'InstanceState' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'code', 'instanceState_code' - The status code for the instance.
--
-- 'name', 'instanceState_name' - The state of the instance (e.g., @running@ or @pending@).
newInstanceState ::
  InstanceState
newInstanceState =
  InstanceState'
    { code = Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | The status code for the instance.
instanceState_code :: Lens.Lens' InstanceState (Prelude.Maybe Prelude.Int)
instanceState_code = Lens.lens (\InstanceState' {code} -> code) (\s@InstanceState' {} a -> s {code = a} :: InstanceState)

-- | The state of the instance (e.g., @running@ or @pending@).
instanceState_name :: Lens.Lens' InstanceState (Prelude.Maybe Prelude.Text)
instanceState_name = Lens.lens (\InstanceState' {name} -> name) (\s@InstanceState' {} a -> s {name = a} :: InstanceState)

instance Prelude.FromJSON InstanceState where
  parseJSON =
    Prelude.withObject
      "InstanceState"
      ( \x ->
          InstanceState'
            Prelude.<$> (x Prelude..:? "code")
            Prelude.<*> (x Prelude..:? "name")
      )

instance Prelude.Hashable InstanceState

instance Prelude.NFData InstanceState
