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
-- Module      : Network.AWS.CloudFormation.Types.AccountLimit
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.AccountLimit where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The AccountLimit data type.
--
-- CloudFormation has the following limits per account:
--
-- -   Number of concurrent resources
--
-- -   Number of stacks
--
-- -   Number of stack outputs
--
-- For more information about these account limits, and other
-- CloudFormation limits, see
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/cloudformation-limits.html AWS CloudFormation Limits>
-- in the /AWS CloudFormation User Guide/.
--
-- /See:/ 'newAccountLimit' smart constructor.
data AccountLimit = AccountLimit'
  { -- | The name of the account limit.
    --
    -- Values: @ConcurrentResourcesLimit@ | @StackLimit@ | @StackOutputsLimit@
    name :: Core.Maybe Core.Text,
    -- | The value that is associated with the account limit name.
    value :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AccountLimit' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'accountLimit_name' - The name of the account limit.
--
-- Values: @ConcurrentResourcesLimit@ | @StackLimit@ | @StackOutputsLimit@
--
-- 'value', 'accountLimit_value' - The value that is associated with the account limit name.
newAccountLimit ::
  AccountLimit
newAccountLimit =
  AccountLimit'
    { name = Core.Nothing,
      value = Core.Nothing
    }

-- | The name of the account limit.
--
-- Values: @ConcurrentResourcesLimit@ | @StackLimit@ | @StackOutputsLimit@
accountLimit_name :: Lens.Lens' AccountLimit (Core.Maybe Core.Text)
accountLimit_name = Lens.lens (\AccountLimit' {name} -> name) (\s@AccountLimit' {} a -> s {name = a} :: AccountLimit)

-- | The value that is associated with the account limit name.
accountLimit_value :: Lens.Lens' AccountLimit (Core.Maybe Core.Int)
accountLimit_value = Lens.lens (\AccountLimit' {value} -> value) (\s@AccountLimit' {} a -> s {value = a} :: AccountLimit)

instance Core.FromXML AccountLimit where
  parseXML x =
    AccountLimit'
      Core.<$> (x Core..@? "Name") Core.<*> (x Core..@? "Value")

instance Core.Hashable AccountLimit

instance Core.NFData AccountLimit
