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
-- Module      : Amazonka.EC2.Types.InstanceState
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.InstanceState where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.InstanceStateName
import qualified Amazonka.Prelude as Prelude

-- | Describes the current state of an instance.
--
-- /See:/ 'newInstanceState' smart constructor.
data InstanceState = InstanceState'
  { -- | The current state of the instance.
    name :: InstanceStateName,
    -- | The state of the instance as a 16-bit unsigned integer.
    --
    -- The high byte is all of the bits between 2^8 and (2^16)-1, which equals
    -- decimal values between 256 and 65,535. These numerical values are used
    -- for internal purposes and should be ignored.
    --
    -- The low byte is all of the bits between 2^0 and (2^8)-1, which equals
    -- decimal values between 0 and 255.
    --
    -- The valid values for instance-state-code will all be in the range of the
    -- low byte and they are:
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
    code :: Prelude.Int
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
-- 'name', 'instanceState_name' - The current state of the instance.
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
-- The valid values for instance-state-code will all be in the range of the
-- low byte and they are:
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
newInstanceState ::
  -- | 'name'
  InstanceStateName ->
  -- | 'code'
  Prelude.Int ->
  InstanceState
newInstanceState pName_ pCode_ =
  InstanceState' {name = pName_, code = pCode_}

-- | The current state of the instance.
instanceState_name :: Lens.Lens' InstanceState InstanceStateName
instanceState_name = Lens.lens (\InstanceState' {name} -> name) (\s@InstanceState' {} a -> s {name = a} :: InstanceState)

-- | The state of the instance as a 16-bit unsigned integer.
--
-- The high byte is all of the bits between 2^8 and (2^16)-1, which equals
-- decimal values between 256 and 65,535. These numerical values are used
-- for internal purposes and should be ignored.
--
-- The low byte is all of the bits between 2^0 and (2^8)-1, which equals
-- decimal values between 0 and 255.
--
-- The valid values for instance-state-code will all be in the range of the
-- low byte and they are:
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
instanceState_code :: Lens.Lens' InstanceState Prelude.Int
instanceState_code = Lens.lens (\InstanceState' {code} -> code) (\s@InstanceState' {} a -> s {code = a} :: InstanceState)

instance Data.FromXML InstanceState where
  parseXML x =
    InstanceState'
      Prelude.<$> (x Data..@ "name")
      Prelude.<*> (x Data..@ "code")

instance Prelude.Hashable InstanceState where
  hashWithSalt _salt InstanceState' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` code

instance Prelude.NFData InstanceState where
  rnf InstanceState' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf code
