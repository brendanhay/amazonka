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
-- Module      : Amazonka.MacieV2.Types.IpCountry
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.IpCountry where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Provides information about the country that an IP address originated
-- from.
--
-- /See:/ 'newIpCountry' smart constructor.
data IpCountry = IpCountry'
  { -- | The name of the country that the IP address originated from.
    name :: Prelude.Maybe Prelude.Text,
    -- | The two-character code, in ISO 3166-1 alpha-2 format, for the country
    -- that the IP address originated from. For example, US for the United
    -- States.
    code :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IpCountry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'ipCountry_name' - The name of the country that the IP address originated from.
--
-- 'code', 'ipCountry_code' - The two-character code, in ISO 3166-1 alpha-2 format, for the country
-- that the IP address originated from. For example, US for the United
-- States.
newIpCountry ::
  IpCountry
newIpCountry =
  IpCountry'
    { name = Prelude.Nothing,
      code = Prelude.Nothing
    }

-- | The name of the country that the IP address originated from.
ipCountry_name :: Lens.Lens' IpCountry (Prelude.Maybe Prelude.Text)
ipCountry_name = Lens.lens (\IpCountry' {name} -> name) (\s@IpCountry' {} a -> s {name = a} :: IpCountry)

-- | The two-character code, in ISO 3166-1 alpha-2 format, for the country
-- that the IP address originated from. For example, US for the United
-- States.
ipCountry_code :: Lens.Lens' IpCountry (Prelude.Maybe Prelude.Text)
ipCountry_code = Lens.lens (\IpCountry' {code} -> code) (\s@IpCountry' {} a -> s {code = a} :: IpCountry)

instance Core.FromJSON IpCountry where
  parseJSON =
    Core.withObject
      "IpCountry"
      ( \x ->
          IpCountry'
            Prelude.<$> (x Core..:? "name") Prelude.<*> (x Core..:? "code")
      )

instance Prelude.Hashable IpCountry where
  hashWithSalt _salt IpCountry' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` code

instance Prelude.NFData IpCountry where
  rnf IpCountry' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf code
