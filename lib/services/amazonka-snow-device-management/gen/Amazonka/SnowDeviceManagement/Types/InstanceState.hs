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
-- Module      : Amazonka.SnowDeviceManagement.Types.InstanceState
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SnowDeviceManagement.Types.InstanceState where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SnowDeviceManagement.Types.InstanceStateName

-- | The description of the current state of an instance.
--
-- /See:/ 'newInstanceState' smart constructor.
data InstanceState = InstanceState'
  { -- | The state of the instance as a 16-bit unsigned integer.
    --
    -- The high byte is all of the bits between 2^8 and (2^16)-1, which equals
    -- decimal values between 256 and 65,535. These numerical values are used
    -- for internal purposes and should be ignored.
    --
    -- The low byte is all of the bits between 2^0 and (2^8)-1, which equals
    -- decimal values between 0 and 255.
    --
    -- The valid values for the instance state code are all in the range of the
    -- low byte. These values are:
    --
    -- -   @0@ : @pending@
    --
    -- -   @16@ : @running@
    --
    -- -   @32@ : @shutting-down@
    --
    -- -   @48@ : @terminated@
    --
    -- -   @64@ : @stopping@
    --
    -- -   @80@ : @stopped@
    --
    -- You can ignore the high byte value by zeroing out all of the bits above
    -- 2^8 or 256 in decimal.
    code :: Prelude.Maybe Prelude.Int,
    -- | The current state of the instance.
    name :: Prelude.Maybe InstanceStateName
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InstanceState' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'code', 'instanceState_code' - The state of the instance as a 16-bit unsigned integer.
--
-- The high byte is all of the bits between 2^8 and (2^16)-1, which equals
-- decimal values between 256 and 65,535. These numerical values are used
-- for internal purposes and should be ignored.
--
-- The low byte is all of the bits between 2^0 and (2^8)-1, which equals
-- decimal values between 0 and 255.
--
-- The valid values for the instance state code are all in the range of the
-- low byte. These values are:
--
-- -   @0@ : @pending@
--
-- -   @16@ : @running@
--
-- -   @32@ : @shutting-down@
--
-- -   @48@ : @terminated@
--
-- -   @64@ : @stopping@
--
-- -   @80@ : @stopped@
--
-- You can ignore the high byte value by zeroing out all of the bits above
-- 2^8 or 256 in decimal.
--
-- 'name', 'instanceState_name' - The current state of the instance.
newInstanceState ::
  InstanceState
newInstanceState =
  InstanceState'
    { code = Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | The state of the instance as a 16-bit unsigned integer.
--
-- The high byte is all of the bits between 2^8 and (2^16)-1, which equals
-- decimal values between 256 and 65,535. These numerical values are used
-- for internal purposes and should be ignored.
--
-- The low byte is all of the bits between 2^0 and (2^8)-1, which equals
-- decimal values between 0 and 255.
--
-- The valid values for the instance state code are all in the range of the
-- low byte. These values are:
--
-- -   @0@ : @pending@
--
-- -   @16@ : @running@
--
-- -   @32@ : @shutting-down@
--
-- -   @48@ : @terminated@
--
-- -   @64@ : @stopping@
--
-- -   @80@ : @stopped@
--
-- You can ignore the high byte value by zeroing out all of the bits above
-- 2^8 or 256 in decimal.
instanceState_code :: Lens.Lens' InstanceState (Prelude.Maybe Prelude.Int)
instanceState_code = Lens.lens (\InstanceState' {code} -> code) (\s@InstanceState' {} a -> s {code = a} :: InstanceState)

-- | The current state of the instance.
instanceState_name :: Lens.Lens' InstanceState (Prelude.Maybe InstanceStateName)
instanceState_name = Lens.lens (\InstanceState' {name} -> name) (\s@InstanceState' {} a -> s {name = a} :: InstanceState)

instance Data.FromJSON InstanceState where
  parseJSON =
    Data.withObject
      "InstanceState"
      ( \x ->
          InstanceState'
            Prelude.<$> (x Data..:? "code") Prelude.<*> (x Data..:? "name")
      )

instance Prelude.Hashable InstanceState where
  hashWithSalt _salt InstanceState' {..} =
    _salt `Prelude.hashWithSalt` code
      `Prelude.hashWithSalt` name

instance Prelude.NFData InstanceState where
  rnf InstanceState' {..} =
    Prelude.rnf code `Prelude.seq` Prelude.rnf name
