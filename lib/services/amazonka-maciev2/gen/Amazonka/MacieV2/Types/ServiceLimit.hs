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
-- Module      : Amazonka.MacieV2.Types.ServiceLimit
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.ServiceLimit where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MacieV2.Types.Unit
import qualified Amazonka.Prelude as Prelude

-- | Specifies a current quota for an Amazon Macie account.
--
-- /See:/ 'newServiceLimit' smart constructor.
data ServiceLimit = ServiceLimit'
  { -- | Specifies whether the account has met the quota that corresponds to the
    -- metric specified by the UsageByAccount.type field in the response.
    isServiceLimited :: Prelude.Maybe Prelude.Bool,
    -- | The unit of measurement for the value specified by the value field.
    unit :: Prelude.Maybe Unit,
    -- | The value for the metric specified by the UsageByAccount.type field in
    -- the response.
    value :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ServiceLimit' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'isServiceLimited', 'serviceLimit_isServiceLimited' - Specifies whether the account has met the quota that corresponds to the
-- metric specified by the UsageByAccount.type field in the response.
--
-- 'unit', 'serviceLimit_unit' - The unit of measurement for the value specified by the value field.
--
-- 'value', 'serviceLimit_value' - The value for the metric specified by the UsageByAccount.type field in
-- the response.
newServiceLimit ::
  ServiceLimit
newServiceLimit =
  ServiceLimit'
    { isServiceLimited = Prelude.Nothing,
      unit = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | Specifies whether the account has met the quota that corresponds to the
-- metric specified by the UsageByAccount.type field in the response.
serviceLimit_isServiceLimited :: Lens.Lens' ServiceLimit (Prelude.Maybe Prelude.Bool)
serviceLimit_isServiceLimited = Lens.lens (\ServiceLimit' {isServiceLimited} -> isServiceLimited) (\s@ServiceLimit' {} a -> s {isServiceLimited = a} :: ServiceLimit)

-- | The unit of measurement for the value specified by the value field.
serviceLimit_unit :: Lens.Lens' ServiceLimit (Prelude.Maybe Unit)
serviceLimit_unit = Lens.lens (\ServiceLimit' {unit} -> unit) (\s@ServiceLimit' {} a -> s {unit = a} :: ServiceLimit)

-- | The value for the metric specified by the UsageByAccount.type field in
-- the response.
serviceLimit_value :: Lens.Lens' ServiceLimit (Prelude.Maybe Prelude.Integer)
serviceLimit_value = Lens.lens (\ServiceLimit' {value} -> value) (\s@ServiceLimit' {} a -> s {value = a} :: ServiceLimit)

instance Data.FromJSON ServiceLimit where
  parseJSON =
    Data.withObject
      "ServiceLimit"
      ( \x ->
          ServiceLimit'
            Prelude.<$> (x Data..:? "isServiceLimited")
            Prelude.<*> (x Data..:? "unit")
            Prelude.<*> (x Data..:? "value")
      )

instance Prelude.Hashable ServiceLimit where
  hashWithSalt _salt ServiceLimit' {..} =
    _salt `Prelude.hashWithSalt` isServiceLimited
      `Prelude.hashWithSalt` unit
      `Prelude.hashWithSalt` value

instance Prelude.NFData ServiceLimit where
  rnf ServiceLimit' {..} =
    Prelude.rnf isServiceLimited
      `Prelude.seq` Prelude.rnf unit
      `Prelude.seq` Prelude.rnf value
