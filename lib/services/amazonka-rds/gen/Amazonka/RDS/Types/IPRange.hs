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
-- Module      : Amazonka.RDS.Types.IPRange
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RDS.Types.IPRange where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | This data type is used as a response element in the
-- @DescribeDBSecurityGroups@ action.
--
-- /See:/ 'newIPRange' smart constructor.
data IPRange = IPRange'
  { -- | Specifies the IP range.
    cidrip :: Prelude.Maybe Prelude.Text,
    -- | Specifies the status of the IP range. Status can be \"authorizing\",
    -- \"authorized\", \"revoking\", and \"revoked\".
    status :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IPRange' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cidrip', 'iPRange_cidrip' - Specifies the IP range.
--
-- 'status', 'iPRange_status' - Specifies the status of the IP range. Status can be \"authorizing\",
-- \"authorized\", \"revoking\", and \"revoked\".
newIPRange ::
  IPRange
newIPRange =
  IPRange'
    { cidrip = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | Specifies the IP range.
iPRange_cidrip :: Lens.Lens' IPRange (Prelude.Maybe Prelude.Text)
iPRange_cidrip = Lens.lens (\IPRange' {cidrip} -> cidrip) (\s@IPRange' {} a -> s {cidrip = a} :: IPRange)

-- | Specifies the status of the IP range. Status can be \"authorizing\",
-- \"authorized\", \"revoking\", and \"revoked\".
iPRange_status :: Lens.Lens' IPRange (Prelude.Maybe Prelude.Text)
iPRange_status = Lens.lens (\IPRange' {status} -> status) (\s@IPRange' {} a -> s {status = a} :: IPRange)

instance Data.FromXML IPRange where
  parseXML x =
    IPRange'
      Prelude.<$> (x Data..@? "CIDRIP")
      Prelude.<*> (x Data..@? "Status")

instance Prelude.Hashable IPRange where
  hashWithSalt _salt IPRange' {..} =
    _salt
      `Prelude.hashWithSalt` cidrip
      `Prelude.hashWithSalt` status

instance Prelude.NFData IPRange where
  rnf IPRange' {..} =
    Prelude.rnf cidrip `Prelude.seq` Prelude.rnf status
