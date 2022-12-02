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
-- Module      : Amazonka.Route53RecoveryReadiness.Types.R53ResourceRecord
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53RecoveryReadiness.Types.R53ResourceRecord where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The Route 53 resource that a DNS target resource record points to.
--
-- /See:/ 'newR53ResourceRecord' smart constructor.
data R53ResourceRecord = R53ResourceRecord'
  { -- | The DNS target domain name.
    domainName :: Prelude.Maybe Prelude.Text,
    -- | The Route 53 Resource Record Set ID.
    recordSetId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'R53ResourceRecord' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainName', 'r53ResourceRecord_domainName' - The DNS target domain name.
--
-- 'recordSetId', 'r53ResourceRecord_recordSetId' - The Route 53 Resource Record Set ID.
newR53ResourceRecord ::
  R53ResourceRecord
newR53ResourceRecord =
  R53ResourceRecord'
    { domainName = Prelude.Nothing,
      recordSetId = Prelude.Nothing
    }

-- | The DNS target domain name.
r53ResourceRecord_domainName :: Lens.Lens' R53ResourceRecord (Prelude.Maybe Prelude.Text)
r53ResourceRecord_domainName = Lens.lens (\R53ResourceRecord' {domainName} -> domainName) (\s@R53ResourceRecord' {} a -> s {domainName = a} :: R53ResourceRecord)

-- | The Route 53 Resource Record Set ID.
r53ResourceRecord_recordSetId :: Lens.Lens' R53ResourceRecord (Prelude.Maybe Prelude.Text)
r53ResourceRecord_recordSetId = Lens.lens (\R53ResourceRecord' {recordSetId} -> recordSetId) (\s@R53ResourceRecord' {} a -> s {recordSetId = a} :: R53ResourceRecord)

instance Data.FromJSON R53ResourceRecord where
  parseJSON =
    Data.withObject
      "R53ResourceRecord"
      ( \x ->
          R53ResourceRecord'
            Prelude.<$> (x Data..:? "domainName")
            Prelude.<*> (x Data..:? "recordSetId")
      )

instance Prelude.Hashable R53ResourceRecord where
  hashWithSalt _salt R53ResourceRecord' {..} =
    _salt `Prelude.hashWithSalt` domainName
      `Prelude.hashWithSalt` recordSetId

instance Prelude.NFData R53ResourceRecord where
  rnf R53ResourceRecord' {..} =
    Prelude.rnf domainName
      `Prelude.seq` Prelude.rnf recordSetId

instance Data.ToJSON R53ResourceRecord where
  toJSON R53ResourceRecord' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("domainName" Data..=) Prelude.<$> domainName,
            ("recordSetId" Data..=) Prelude.<$> recordSetId
          ]
      )
