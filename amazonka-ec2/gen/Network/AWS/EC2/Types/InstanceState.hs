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
-- Module      : Network.AWS.EC2.Types.InstanceState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.InstanceState where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.InstanceStateName
import qualified Network.AWS.Lens as Lens

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
    code :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
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
instanceState_code :: Lens.Lens' InstanceState Core.Int
instanceState_code = Lens.lens (\InstanceState' {code} -> code) (\s@InstanceState' {} a -> s {code = a} :: InstanceState)

instance Core.FromXML InstanceState where
  parseXML x =
    InstanceState'
      Core.<$> (x Core..@ "name") Core.<*> (x Core..@ "code")

instance Core.Hashable InstanceState

instance Core.NFData InstanceState
