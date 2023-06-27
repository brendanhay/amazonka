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
-- Module      : Amazonka.CloudFormation.Types.AccountLimit
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFormation.Types.AccountLimit where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

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
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/cloudformation-limits.html CloudFormation quotas>
-- in the /CloudFormation User Guide/.
--
-- /See:/ 'newAccountLimit' smart constructor.
data AccountLimit = AccountLimit'
  { -- | The name of the account limit.
    --
    -- Values: @ConcurrentResourcesLimit@ | @StackLimit@ | @StackOutputsLimit@
    name :: Prelude.Maybe Prelude.Text,
    -- | The value that\'s associated with the account limit name.
    value :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'value', 'accountLimit_value' - The value that\'s associated with the account limit name.
newAccountLimit ::
  AccountLimit
newAccountLimit =
  AccountLimit'
    { name = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | The name of the account limit.
--
-- Values: @ConcurrentResourcesLimit@ | @StackLimit@ | @StackOutputsLimit@
accountLimit_name :: Lens.Lens' AccountLimit (Prelude.Maybe Prelude.Text)
accountLimit_name = Lens.lens (\AccountLimit' {name} -> name) (\s@AccountLimit' {} a -> s {name = a} :: AccountLimit)

-- | The value that\'s associated with the account limit name.
accountLimit_value :: Lens.Lens' AccountLimit (Prelude.Maybe Prelude.Int)
accountLimit_value = Lens.lens (\AccountLimit' {value} -> value) (\s@AccountLimit' {} a -> s {value = a} :: AccountLimit)

instance Data.FromXML AccountLimit where
  parseXML x =
    AccountLimit'
      Prelude.<$> (x Data..@? "Name")
      Prelude.<*> (x Data..@? "Value")

instance Prelude.Hashable AccountLimit where
  hashWithSalt _salt AccountLimit' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` value

instance Prelude.NFData AccountLimit where
  rnf AccountLimit' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf value
