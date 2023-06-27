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
-- Module      : Amazonka.WorkSpacesWeb.Types.IpRule
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkSpacesWeb.Types.IpRule where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The IP rules of the IP access settings.
--
-- /See:/ 'newIpRule' smart constructor.
data IpRule = IpRule'
  { -- | The description of the IP rule.
    description :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The IP range of the IP rule.
    ipRange :: Data.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IpRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'ipRule_description' - The description of the IP rule.
--
-- 'ipRange', 'ipRule_ipRange' - The IP range of the IP rule.
newIpRule ::
  -- | 'ipRange'
  Prelude.Text ->
  IpRule
newIpRule pIpRange_ =
  IpRule'
    { description = Prelude.Nothing,
      ipRange = Data._Sensitive Lens.# pIpRange_
    }

-- | The description of the IP rule.
ipRule_description :: Lens.Lens' IpRule (Prelude.Maybe Prelude.Text)
ipRule_description = Lens.lens (\IpRule' {description} -> description) (\s@IpRule' {} a -> s {description = a} :: IpRule) Prelude.. Lens.mapping Data._Sensitive

-- | The IP range of the IP rule.
ipRule_ipRange :: Lens.Lens' IpRule Prelude.Text
ipRule_ipRange = Lens.lens (\IpRule' {ipRange} -> ipRange) (\s@IpRule' {} a -> s {ipRange = a} :: IpRule) Prelude.. Data._Sensitive

instance Data.FromJSON IpRule where
  parseJSON =
    Data.withObject
      "IpRule"
      ( \x ->
          IpRule'
            Prelude.<$> (x Data..:? "description")
            Prelude.<*> (x Data..: "ipRange")
      )

instance Prelude.Hashable IpRule where
  hashWithSalt _salt IpRule' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` ipRange

instance Prelude.NFData IpRule where
  rnf IpRule' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf ipRange

instance Data.ToJSON IpRule where
  toJSON IpRule' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("description" Data..=) Prelude.<$> description,
            Prelude.Just ("ipRange" Data..= ipRange)
          ]
      )
