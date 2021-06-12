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
-- Module      : Network.AWS.RDS.Types.IPRange
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.IPRange where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | This data type is used as a response element in the
-- @DescribeDBSecurityGroups@ action.
--
-- /See:/ 'newIPRange' smart constructor.
data IPRange = IPRange'
  { -- | Specifies the status of the IP range. Status can be \"authorizing\",
    -- \"authorized\", \"revoking\", and \"revoked\".
    status :: Core.Maybe Core.Text,
    -- | Specifies the IP range.
    cidrip :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'IPRange' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'iPRange_status' - Specifies the status of the IP range. Status can be \"authorizing\",
-- \"authorized\", \"revoking\", and \"revoked\".
--
-- 'cidrip', 'iPRange_cidrip' - Specifies the IP range.
newIPRange ::
  IPRange
newIPRange =
  IPRange'
    { status = Core.Nothing,
      cidrip = Core.Nothing
    }

-- | Specifies the status of the IP range. Status can be \"authorizing\",
-- \"authorized\", \"revoking\", and \"revoked\".
iPRange_status :: Lens.Lens' IPRange (Core.Maybe Core.Text)
iPRange_status = Lens.lens (\IPRange' {status} -> status) (\s@IPRange' {} a -> s {status = a} :: IPRange)

-- | Specifies the IP range.
iPRange_cidrip :: Lens.Lens' IPRange (Core.Maybe Core.Text)
iPRange_cidrip = Lens.lens (\IPRange' {cidrip} -> cidrip) (\s@IPRange' {} a -> s {cidrip = a} :: IPRange)

instance Core.FromXML IPRange where
  parseXML x =
    IPRange'
      Core.<$> (x Core..@? "Status") Core.<*> (x Core..@? "CIDRIP")

instance Core.Hashable IPRange

instance Core.NFData IPRange
