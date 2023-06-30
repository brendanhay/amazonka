{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.SecurityHub.UpdateFindingAggregator
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the finding aggregation configuration. Used to update the Region
-- linking mode and the list of included or excluded Regions. You cannot
-- use @UpdateFindingAggregator@ to change the aggregation Region.
--
-- You must run @UpdateFindingAggregator@ from the current aggregation
-- Region.
module Amazonka.SecurityHub.UpdateFindingAggregator
  ( -- * Creating a Request
    UpdateFindingAggregator (..),
    newUpdateFindingAggregator,

    -- * Request Lenses
    updateFindingAggregator_regions,
    updateFindingAggregator_findingAggregatorArn,
    updateFindingAggregator_regionLinkingMode,

    -- * Destructuring the Response
    UpdateFindingAggregatorResponse (..),
    newUpdateFindingAggregatorResponse,

    -- * Response Lenses
    updateFindingAggregatorResponse_findingAggregationRegion,
    updateFindingAggregatorResponse_findingAggregatorArn,
    updateFindingAggregatorResponse_regionLinkingMode,
    updateFindingAggregatorResponse_regions,
    updateFindingAggregatorResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SecurityHub.Types

-- | /See:/ 'newUpdateFindingAggregator' smart constructor.
data UpdateFindingAggregator = UpdateFindingAggregator'
  { -- | If @RegionLinkingMode@ is @ALL_REGIONS_EXCEPT_SPECIFIED@, then this is a
    -- space-separated list of Regions that do not aggregate findings to the
    -- aggregation Region.
    --
    -- If @RegionLinkingMode@ is @SPECIFIED_REGIONS@, then this is a
    -- space-separated list of Regions that do aggregate findings to the
    -- aggregation Region.
    regions :: Prelude.Maybe [Prelude.Text],
    -- | The ARN of the finding aggregator. To obtain the ARN, use
    -- @ListFindingAggregators@.
    findingAggregatorArn :: Prelude.Text,
    -- | Indicates whether to aggregate findings from all of the available
    -- Regions in the current partition. Also determines whether to
    -- automatically aggregate findings from new Regions as Security Hub
    -- supports them and you opt into them.
    --
    -- The selected option also determines how to use the Regions provided in
    -- the Regions list.
    --
    -- The options are as follows:
    --
    -- -   @ALL_REGIONS@ - Indicates to aggregate findings from all of the
    --     Regions where Security Hub is enabled. When you choose this option,
    --     Security Hub also automatically aggregates findings from new Regions
    --     as Security Hub supports them and you opt into them.
    --
    -- -   @ALL_REGIONS_EXCEPT_SPECIFIED@ - Indicates to aggregate findings
    --     from all of the Regions where Security Hub is enabled, except for
    --     the Regions listed in the @Regions@ parameter. When you choose this
    --     option, Security Hub also automatically aggregates findings from new
    --     Regions as Security Hub supports them and you opt into them.
    --
    -- -   @SPECIFIED_REGIONS@ - Indicates to aggregate findings only from the
    --     Regions listed in the @Regions@ parameter. Security Hub does not
    --     automatically aggregate findings from new Regions.
    regionLinkingMode :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateFindingAggregator' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'regions', 'updateFindingAggregator_regions' - If @RegionLinkingMode@ is @ALL_REGIONS_EXCEPT_SPECIFIED@, then this is a
-- space-separated list of Regions that do not aggregate findings to the
-- aggregation Region.
--
-- If @RegionLinkingMode@ is @SPECIFIED_REGIONS@, then this is a
-- space-separated list of Regions that do aggregate findings to the
-- aggregation Region.
--
-- 'findingAggregatorArn', 'updateFindingAggregator_findingAggregatorArn' - The ARN of the finding aggregator. To obtain the ARN, use
-- @ListFindingAggregators@.
--
-- 'regionLinkingMode', 'updateFindingAggregator_regionLinkingMode' - Indicates whether to aggregate findings from all of the available
-- Regions in the current partition. Also determines whether to
-- automatically aggregate findings from new Regions as Security Hub
-- supports them and you opt into them.
--
-- The selected option also determines how to use the Regions provided in
-- the Regions list.
--
-- The options are as follows:
--
-- -   @ALL_REGIONS@ - Indicates to aggregate findings from all of the
--     Regions where Security Hub is enabled. When you choose this option,
--     Security Hub also automatically aggregates findings from new Regions
--     as Security Hub supports them and you opt into them.
--
-- -   @ALL_REGIONS_EXCEPT_SPECIFIED@ - Indicates to aggregate findings
--     from all of the Regions where Security Hub is enabled, except for
--     the Regions listed in the @Regions@ parameter. When you choose this
--     option, Security Hub also automatically aggregates findings from new
--     Regions as Security Hub supports them and you opt into them.
--
-- -   @SPECIFIED_REGIONS@ - Indicates to aggregate findings only from the
--     Regions listed in the @Regions@ parameter. Security Hub does not
--     automatically aggregate findings from new Regions.
newUpdateFindingAggregator ::
  -- | 'findingAggregatorArn'
  Prelude.Text ->
  -- | 'regionLinkingMode'
  Prelude.Text ->
  UpdateFindingAggregator
newUpdateFindingAggregator
  pFindingAggregatorArn_
  pRegionLinkingMode_ =
    UpdateFindingAggregator'
      { regions = Prelude.Nothing,
        findingAggregatorArn = pFindingAggregatorArn_,
        regionLinkingMode = pRegionLinkingMode_
      }

-- | If @RegionLinkingMode@ is @ALL_REGIONS_EXCEPT_SPECIFIED@, then this is a
-- space-separated list of Regions that do not aggregate findings to the
-- aggregation Region.
--
-- If @RegionLinkingMode@ is @SPECIFIED_REGIONS@, then this is a
-- space-separated list of Regions that do aggregate findings to the
-- aggregation Region.
updateFindingAggregator_regions :: Lens.Lens' UpdateFindingAggregator (Prelude.Maybe [Prelude.Text])
updateFindingAggregator_regions = Lens.lens (\UpdateFindingAggregator' {regions} -> regions) (\s@UpdateFindingAggregator' {} a -> s {regions = a} :: UpdateFindingAggregator) Prelude.. Lens.mapping Lens.coerced

-- | The ARN of the finding aggregator. To obtain the ARN, use
-- @ListFindingAggregators@.
updateFindingAggregator_findingAggregatorArn :: Lens.Lens' UpdateFindingAggregator Prelude.Text
updateFindingAggregator_findingAggregatorArn = Lens.lens (\UpdateFindingAggregator' {findingAggregatorArn} -> findingAggregatorArn) (\s@UpdateFindingAggregator' {} a -> s {findingAggregatorArn = a} :: UpdateFindingAggregator)

-- | Indicates whether to aggregate findings from all of the available
-- Regions in the current partition. Also determines whether to
-- automatically aggregate findings from new Regions as Security Hub
-- supports them and you opt into them.
--
-- The selected option also determines how to use the Regions provided in
-- the Regions list.
--
-- The options are as follows:
--
-- -   @ALL_REGIONS@ - Indicates to aggregate findings from all of the
--     Regions where Security Hub is enabled. When you choose this option,
--     Security Hub also automatically aggregates findings from new Regions
--     as Security Hub supports them and you opt into them.
--
-- -   @ALL_REGIONS_EXCEPT_SPECIFIED@ - Indicates to aggregate findings
--     from all of the Regions where Security Hub is enabled, except for
--     the Regions listed in the @Regions@ parameter. When you choose this
--     option, Security Hub also automatically aggregates findings from new
--     Regions as Security Hub supports them and you opt into them.
--
-- -   @SPECIFIED_REGIONS@ - Indicates to aggregate findings only from the
--     Regions listed in the @Regions@ parameter. Security Hub does not
--     automatically aggregate findings from new Regions.
updateFindingAggregator_regionLinkingMode :: Lens.Lens' UpdateFindingAggregator Prelude.Text
updateFindingAggregator_regionLinkingMode = Lens.lens (\UpdateFindingAggregator' {regionLinkingMode} -> regionLinkingMode) (\s@UpdateFindingAggregator' {} a -> s {regionLinkingMode = a} :: UpdateFindingAggregator)

instance Core.AWSRequest UpdateFindingAggregator where
  type
    AWSResponse UpdateFindingAggregator =
      UpdateFindingAggregatorResponse
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateFindingAggregatorResponse'
            Prelude.<$> (x Data..?> "FindingAggregationRegion")
            Prelude.<*> (x Data..?> "FindingAggregatorArn")
            Prelude.<*> (x Data..?> "RegionLinkingMode")
            Prelude.<*> (x Data..?> "Regions" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateFindingAggregator where
  hashWithSalt _salt UpdateFindingAggregator' {..} =
    _salt
      `Prelude.hashWithSalt` regions
      `Prelude.hashWithSalt` findingAggregatorArn
      `Prelude.hashWithSalt` regionLinkingMode

instance Prelude.NFData UpdateFindingAggregator where
  rnf UpdateFindingAggregator' {..} =
    Prelude.rnf regions
      `Prelude.seq` Prelude.rnf findingAggregatorArn
      `Prelude.seq` Prelude.rnf regionLinkingMode

instance Data.ToHeaders UpdateFindingAggregator where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateFindingAggregator where
  toJSON UpdateFindingAggregator' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Regions" Data..=) Prelude.<$> regions,
            Prelude.Just
              ( "FindingAggregatorArn"
                  Data..= findingAggregatorArn
              ),
            Prelude.Just
              ("RegionLinkingMode" Data..= regionLinkingMode)
          ]
      )

instance Data.ToPath UpdateFindingAggregator where
  toPath = Prelude.const "/findingAggregator/update"

instance Data.ToQuery UpdateFindingAggregator where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateFindingAggregatorResponse' smart constructor.
data UpdateFindingAggregatorResponse = UpdateFindingAggregatorResponse'
  { -- | The aggregation Region.
    findingAggregationRegion :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the finding aggregator.
    findingAggregatorArn :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether to link all Regions, all Regions except for a list of
    -- excluded Regions, or a list of included Regions.
    regionLinkingMode :: Prelude.Maybe Prelude.Text,
    -- | The list of excluded Regions or included Regions.
    regions :: Prelude.Maybe [Prelude.Text],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateFindingAggregatorResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'findingAggregationRegion', 'updateFindingAggregatorResponse_findingAggregationRegion' - The aggregation Region.
--
-- 'findingAggregatorArn', 'updateFindingAggregatorResponse_findingAggregatorArn' - The ARN of the finding aggregator.
--
-- 'regionLinkingMode', 'updateFindingAggregatorResponse_regionLinkingMode' - Indicates whether to link all Regions, all Regions except for a list of
-- excluded Regions, or a list of included Regions.
--
-- 'regions', 'updateFindingAggregatorResponse_regions' - The list of excluded Regions or included Regions.
--
-- 'httpStatus', 'updateFindingAggregatorResponse_httpStatus' - The response's http status code.
newUpdateFindingAggregatorResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateFindingAggregatorResponse
newUpdateFindingAggregatorResponse pHttpStatus_ =
  UpdateFindingAggregatorResponse'
    { findingAggregationRegion =
        Prelude.Nothing,
      findingAggregatorArn = Prelude.Nothing,
      regionLinkingMode = Prelude.Nothing,
      regions = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The aggregation Region.
updateFindingAggregatorResponse_findingAggregationRegion :: Lens.Lens' UpdateFindingAggregatorResponse (Prelude.Maybe Prelude.Text)
updateFindingAggregatorResponse_findingAggregationRegion = Lens.lens (\UpdateFindingAggregatorResponse' {findingAggregationRegion} -> findingAggregationRegion) (\s@UpdateFindingAggregatorResponse' {} a -> s {findingAggregationRegion = a} :: UpdateFindingAggregatorResponse)

-- | The ARN of the finding aggregator.
updateFindingAggregatorResponse_findingAggregatorArn :: Lens.Lens' UpdateFindingAggregatorResponse (Prelude.Maybe Prelude.Text)
updateFindingAggregatorResponse_findingAggregatorArn = Lens.lens (\UpdateFindingAggregatorResponse' {findingAggregatorArn} -> findingAggregatorArn) (\s@UpdateFindingAggregatorResponse' {} a -> s {findingAggregatorArn = a} :: UpdateFindingAggregatorResponse)

-- | Indicates whether to link all Regions, all Regions except for a list of
-- excluded Regions, or a list of included Regions.
updateFindingAggregatorResponse_regionLinkingMode :: Lens.Lens' UpdateFindingAggregatorResponse (Prelude.Maybe Prelude.Text)
updateFindingAggregatorResponse_regionLinkingMode = Lens.lens (\UpdateFindingAggregatorResponse' {regionLinkingMode} -> regionLinkingMode) (\s@UpdateFindingAggregatorResponse' {} a -> s {regionLinkingMode = a} :: UpdateFindingAggregatorResponse)

-- | The list of excluded Regions or included Regions.
updateFindingAggregatorResponse_regions :: Lens.Lens' UpdateFindingAggregatorResponse (Prelude.Maybe [Prelude.Text])
updateFindingAggregatorResponse_regions = Lens.lens (\UpdateFindingAggregatorResponse' {regions} -> regions) (\s@UpdateFindingAggregatorResponse' {} a -> s {regions = a} :: UpdateFindingAggregatorResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
updateFindingAggregatorResponse_httpStatus :: Lens.Lens' UpdateFindingAggregatorResponse Prelude.Int
updateFindingAggregatorResponse_httpStatus = Lens.lens (\UpdateFindingAggregatorResponse' {httpStatus} -> httpStatus) (\s@UpdateFindingAggregatorResponse' {} a -> s {httpStatus = a} :: UpdateFindingAggregatorResponse)

instance
  Prelude.NFData
    UpdateFindingAggregatorResponse
  where
  rnf UpdateFindingAggregatorResponse' {..} =
    Prelude.rnf findingAggregationRegion
      `Prelude.seq` Prelude.rnf findingAggregatorArn
      `Prelude.seq` Prelude.rnf regionLinkingMode
      `Prelude.seq` Prelude.rnf regions
      `Prelude.seq` Prelude.rnf httpStatus
