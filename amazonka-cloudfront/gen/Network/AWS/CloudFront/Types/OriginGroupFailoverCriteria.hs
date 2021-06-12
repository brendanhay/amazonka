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
-- Module      : Network.AWS.CloudFront.Types.OriginGroupFailoverCriteria
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.OriginGroupFailoverCriteria where

import Network.AWS.CloudFront.Types.StatusCodes
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A complex data type that includes information about the failover
-- criteria for an origin group, including the status codes for which
-- CloudFront will failover from the primary origin to the second origin.
--
-- /See:/ 'newOriginGroupFailoverCriteria' smart constructor.
data OriginGroupFailoverCriteria = OriginGroupFailoverCriteria'
  { -- | The status codes that, when returned from the primary origin, will
    -- trigger CloudFront to failover to the second origin.
    statusCodes :: StatusCodes
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'OriginGroupFailoverCriteria' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'statusCodes', 'originGroupFailoverCriteria_statusCodes' - The status codes that, when returned from the primary origin, will
-- trigger CloudFront to failover to the second origin.
newOriginGroupFailoverCriteria ::
  -- | 'statusCodes'
  StatusCodes ->
  OriginGroupFailoverCriteria
newOriginGroupFailoverCriteria pStatusCodes_ =
  OriginGroupFailoverCriteria'
    { statusCodes =
        pStatusCodes_
    }

-- | The status codes that, when returned from the primary origin, will
-- trigger CloudFront to failover to the second origin.
originGroupFailoverCriteria_statusCodes :: Lens.Lens' OriginGroupFailoverCriteria StatusCodes
originGroupFailoverCriteria_statusCodes = Lens.lens (\OriginGroupFailoverCriteria' {statusCodes} -> statusCodes) (\s@OriginGroupFailoverCriteria' {} a -> s {statusCodes = a} :: OriginGroupFailoverCriteria)

instance Core.FromXML OriginGroupFailoverCriteria where
  parseXML x =
    OriginGroupFailoverCriteria'
      Core.<$> (x Core..@ "StatusCodes")

instance Core.Hashable OriginGroupFailoverCriteria

instance Core.NFData OriginGroupFailoverCriteria

instance Core.ToXML OriginGroupFailoverCriteria where
  toXML OriginGroupFailoverCriteria' {..} =
    Core.mconcat ["StatusCodes" Core.@= statusCodes]
