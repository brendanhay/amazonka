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
-- Module      : Amazonka.DynamoDB.Types.PointInTimeRecoverySpecification
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DynamoDB.Types.PointInTimeRecoverySpecification where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DynamoDB.Types.AttributeValue
import Amazonka.DynamoDB.Types.WriteRequest
import qualified Amazonka.Prelude as Prelude

-- | Represents the settings used to enable point in time recovery.
--
-- /See:/ 'newPointInTimeRecoverySpecification' smart constructor.
data PointInTimeRecoverySpecification = PointInTimeRecoverySpecification'
  { -- | Indicates whether point in time recovery is enabled (true) or disabled
    -- (false) on the table.
    pointInTimeRecoveryEnabled :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PointInTimeRecoverySpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pointInTimeRecoveryEnabled', 'pointInTimeRecoverySpecification_pointInTimeRecoveryEnabled' - Indicates whether point in time recovery is enabled (true) or disabled
-- (false) on the table.
newPointInTimeRecoverySpecification ::
  -- | 'pointInTimeRecoveryEnabled'
  Prelude.Bool ->
  PointInTimeRecoverySpecification
newPointInTimeRecoverySpecification
  pPointInTimeRecoveryEnabled_ =
    PointInTimeRecoverySpecification'
      { pointInTimeRecoveryEnabled =
          pPointInTimeRecoveryEnabled_
      }

-- | Indicates whether point in time recovery is enabled (true) or disabled
-- (false) on the table.
pointInTimeRecoverySpecification_pointInTimeRecoveryEnabled :: Lens.Lens' PointInTimeRecoverySpecification Prelude.Bool
pointInTimeRecoverySpecification_pointInTimeRecoveryEnabled = Lens.lens (\PointInTimeRecoverySpecification' {pointInTimeRecoveryEnabled} -> pointInTimeRecoveryEnabled) (\s@PointInTimeRecoverySpecification' {} a -> s {pointInTimeRecoveryEnabled = a} :: PointInTimeRecoverySpecification)

instance
  Prelude.Hashable
    PointInTimeRecoverySpecification
  where
  hashWithSalt
    _salt
    PointInTimeRecoverySpecification' {..} =
      _salt
        `Prelude.hashWithSalt` pointInTimeRecoveryEnabled

instance
  Prelude.NFData
    PointInTimeRecoverySpecification
  where
  rnf PointInTimeRecoverySpecification' {..} =
    Prelude.rnf pointInTimeRecoveryEnabled

instance Core.ToJSON PointInTimeRecoverySpecification where
  toJSON PointInTimeRecoverySpecification' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "PointInTimeRecoveryEnabled"
                  Core..= pointInTimeRecoveryEnabled
              )
          ]
      )
