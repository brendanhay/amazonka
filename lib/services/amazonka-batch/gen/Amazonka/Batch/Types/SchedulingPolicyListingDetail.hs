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
-- Module      : Amazonka.Batch.Types.SchedulingPolicyListingDetail
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Batch.Types.SchedulingPolicyListingDetail where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object that contains the details of a scheduling policy that\'s
-- returned in a @ListSchedulingPolicy@ action.
--
-- /See:/ 'newSchedulingPolicyListingDetail' smart constructor.
data SchedulingPolicyListingDetail = SchedulingPolicyListingDetail'
  { -- | Amazon Resource Name (ARN) of the scheduling policy.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SchedulingPolicyListingDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'schedulingPolicyListingDetail_arn' - Amazon Resource Name (ARN) of the scheduling policy.
newSchedulingPolicyListingDetail ::
  -- | 'arn'
  Prelude.Text ->
  SchedulingPolicyListingDetail
newSchedulingPolicyListingDetail pArn_ =
  SchedulingPolicyListingDetail' {arn = pArn_}

-- | Amazon Resource Name (ARN) of the scheduling policy.
schedulingPolicyListingDetail_arn :: Lens.Lens' SchedulingPolicyListingDetail Prelude.Text
schedulingPolicyListingDetail_arn = Lens.lens (\SchedulingPolicyListingDetail' {arn} -> arn) (\s@SchedulingPolicyListingDetail' {} a -> s {arn = a} :: SchedulingPolicyListingDetail)

instance Data.FromJSON SchedulingPolicyListingDetail where
  parseJSON =
    Data.withObject
      "SchedulingPolicyListingDetail"
      ( \x ->
          SchedulingPolicyListingDetail'
            Prelude.<$> (x Data..: "arn")
      )

instance
  Prelude.Hashable
    SchedulingPolicyListingDetail
  where
  hashWithSalt _salt SchedulingPolicyListingDetail' {..} =
    _salt `Prelude.hashWithSalt` arn

instance Prelude.NFData SchedulingPolicyListingDetail where
  rnf SchedulingPolicyListingDetail' {..} =
    Prelude.rnf arn
