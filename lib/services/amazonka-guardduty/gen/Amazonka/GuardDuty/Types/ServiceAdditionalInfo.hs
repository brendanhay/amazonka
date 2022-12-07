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
-- Module      : Amazonka.GuardDuty.Types.ServiceAdditionalInfo
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.ServiceAdditionalInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Additional information about the generated finding.
--
-- /See:/ 'newServiceAdditionalInfo' smart constructor.
data ServiceAdditionalInfo = ServiceAdditionalInfo'
  { -- | Describes the type of the additional information.
    type' :: Prelude.Maybe Prelude.Text,
    -- | This field specifies the value of the additional information.
    value :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ServiceAdditionalInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'type'', 'serviceAdditionalInfo_type' - Describes the type of the additional information.
--
-- 'value', 'serviceAdditionalInfo_value' - This field specifies the value of the additional information.
newServiceAdditionalInfo ::
  ServiceAdditionalInfo
newServiceAdditionalInfo =
  ServiceAdditionalInfo'
    { type' = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | Describes the type of the additional information.
serviceAdditionalInfo_type :: Lens.Lens' ServiceAdditionalInfo (Prelude.Maybe Prelude.Text)
serviceAdditionalInfo_type = Lens.lens (\ServiceAdditionalInfo' {type'} -> type') (\s@ServiceAdditionalInfo' {} a -> s {type' = a} :: ServiceAdditionalInfo)

-- | This field specifies the value of the additional information.
serviceAdditionalInfo_value :: Lens.Lens' ServiceAdditionalInfo (Prelude.Maybe Prelude.Text)
serviceAdditionalInfo_value = Lens.lens (\ServiceAdditionalInfo' {value} -> value) (\s@ServiceAdditionalInfo' {} a -> s {value = a} :: ServiceAdditionalInfo)

instance Data.FromJSON ServiceAdditionalInfo where
  parseJSON =
    Data.withObject
      "ServiceAdditionalInfo"
      ( \x ->
          ServiceAdditionalInfo'
            Prelude.<$> (x Data..:? "type") Prelude.<*> (x Data..:? "value")
      )

instance Prelude.Hashable ServiceAdditionalInfo where
  hashWithSalt _salt ServiceAdditionalInfo' {..} =
    _salt `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` value

instance Prelude.NFData ServiceAdditionalInfo where
  rnf ServiceAdditionalInfo' {..} =
    Prelude.rnf type' `Prelude.seq` Prelude.rnf value
