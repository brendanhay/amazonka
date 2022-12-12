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
-- Module      : Amazonka.SecurityHub.CreateFindingAggregator
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Used to enable finding aggregation. Must be called from the aggregation
-- Region.
--
-- For more details about cross-Region replication, see
-- <https://docs.aws.amazon.com/securityhub/latest/userguide/finding-aggregation.html Configuring finding aggregation>
-- in the /Security Hub User Guide/.
module Amazonka.SecurityHub.CreateFindingAggregator
  ( -- * Creating a Request
    CreateFindingAggregator (..),
    newCreateFindingAggregator,

    -- * Request Lenses
    createFindingAggregator_regions,
    createFindingAggregator_regionLinkingMode,

    -- * Destructuring the Response
    CreateFindingAggregatorResponse (..),
    newCreateFindingAggregatorResponse,

    -- * Response Lenses
    createFindingAggregatorResponse_findingAggregationRegion,
    createFindingAggregatorResponse_findingAggregatorArn,
    createFindingAggregatorResponse_regionLinkingMode,
    createFindingAggregatorResponse_regions,
    createFindingAggregatorResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SecurityHub.Types

-- | /See:/ 'newCreateFindingAggregator' smart constructor.
data CreateFindingAggregator = CreateFindingAggregator'
  { -- | If @RegionLinkingMode@ is @ALL_REGIONS_EXCEPT_SPECIFIED@, then this is a
    -- space-separated list of Regions that do not aggregate findings to the
    -- aggregation Region.
    --
    -- If @RegionLinkingMode@ is @SPECIFIED_REGIONS@, then this is a
    -- space-separated list of Regions that do aggregate findings to the
    -- aggregation Region.
    regions :: Prelude.Maybe [Prelude.Text],
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
-- Create a value of 'CreateFindingAggregator' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'regions', 'createFindingAggregator_regions' - If @RegionLinkingMode@ is @ALL_REGIONS_EXCEPT_SPECIFIED@, then this is a
-- space-separated list of Regions that do not aggregate findings to the
-- aggregation Region.
--
-- If @RegionLinkingMode@ is @SPECIFIED_REGIONS@, then this is a
-- space-separated list of Regions that do aggregate findings to the
-- aggregation Region.
--
-- 'regionLinkingMode', 'createFindingAggregator_regionLinkingMode' - Indicates whether to aggregate findings from all of the available
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
newCreateFindingAggregator ::
  -- | 'regionLinkingMode'
  Prelude.Text ->
  CreateFindingAggregator
newCreateFindingAggregator pRegionLinkingMode_ =
  CreateFindingAggregator'
    { regions = Prelude.Nothing,
      regionLinkingMode = pRegionLinkingMode_
    }

-- | If @RegionLinkingMode@ is @ALL_REGIONS_EXCEPT_SPECIFIED@, then this is a
-- space-separated list of Regions that do not aggregate findings to the
-- aggregation Region.
--
-- If @RegionLinkingMode@ is @SPECIFIED_REGIONS@, then this is a
-- space-separated list of Regions that do aggregate findings to the
-- aggregation Region.
createFindingAggregator_regions :: Lens.Lens' CreateFindingAggregator (Prelude.Maybe [Prelude.Text])
createFindingAggregator_regions = Lens.lens (\CreateFindingAggregator' {regions} -> regions) (\s@CreateFindingAggregator' {} a -> s {regions = a} :: CreateFindingAggregator) Prelude.. Lens.mapping Lens.coerced

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
createFindingAggregator_regionLinkingMode :: Lens.Lens' CreateFindingAggregator Prelude.Text
createFindingAggregator_regionLinkingMode = Lens.lens (\CreateFindingAggregator' {regionLinkingMode} -> regionLinkingMode) (\s@CreateFindingAggregator' {} a -> s {regionLinkingMode = a} :: CreateFindingAggregator)

instance Core.AWSRequest CreateFindingAggregator where
  type
    AWSResponse CreateFindingAggregator =
      CreateFindingAggregatorResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateFindingAggregatorResponse'
            Prelude.<$> (x Data..?> "FindingAggregationRegion")
            Prelude.<*> (x Data..?> "FindingAggregatorArn")
            Prelude.<*> (x Data..?> "RegionLinkingMode")
            Prelude.<*> (x Data..?> "Regions" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateFindingAggregator where
  hashWithSalt _salt CreateFindingAggregator' {..} =
    _salt `Prelude.hashWithSalt` regions
      `Prelude.hashWithSalt` regionLinkingMode

instance Prelude.NFData CreateFindingAggregator where
  rnf CreateFindingAggregator' {..} =
    Prelude.rnf regions
      `Prelude.seq` Prelude.rnf regionLinkingMode

instance Data.ToHeaders CreateFindingAggregator where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateFindingAggregator where
  toJSON CreateFindingAggregator' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Regions" Data..=) Prelude.<$> regions,
            Prelude.Just
              ("RegionLinkingMode" Data..= regionLinkingMode)
          ]
      )

instance Data.ToPath CreateFindingAggregator where
  toPath = Prelude.const "/findingAggregator/create"

instance Data.ToQuery CreateFindingAggregator where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateFindingAggregatorResponse' smart constructor.
data CreateFindingAggregatorResponse = CreateFindingAggregatorResponse'
  { -- | The aggregation Region.
    findingAggregationRegion :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the finding aggregator. You use the finding aggregator ARN to
    -- retrieve details for, update, and stop finding aggregation.
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
-- Create a value of 'CreateFindingAggregatorResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'findingAggregationRegion', 'createFindingAggregatorResponse_findingAggregationRegion' - The aggregation Region.
--
-- 'findingAggregatorArn', 'createFindingAggregatorResponse_findingAggregatorArn' - The ARN of the finding aggregator. You use the finding aggregator ARN to
-- retrieve details for, update, and stop finding aggregation.
--
-- 'regionLinkingMode', 'createFindingAggregatorResponse_regionLinkingMode' - Indicates whether to link all Regions, all Regions except for a list of
-- excluded Regions, or a list of included Regions.
--
-- 'regions', 'createFindingAggregatorResponse_regions' - The list of excluded Regions or included Regions.
--
-- 'httpStatus', 'createFindingAggregatorResponse_httpStatus' - The response's http status code.
newCreateFindingAggregatorResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateFindingAggregatorResponse
newCreateFindingAggregatorResponse pHttpStatus_ =
  CreateFindingAggregatorResponse'
    { findingAggregationRegion =
        Prelude.Nothing,
      findingAggregatorArn = Prelude.Nothing,
      regionLinkingMode = Prelude.Nothing,
      regions = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The aggregation Region.
createFindingAggregatorResponse_findingAggregationRegion :: Lens.Lens' CreateFindingAggregatorResponse (Prelude.Maybe Prelude.Text)
createFindingAggregatorResponse_findingAggregationRegion = Lens.lens (\CreateFindingAggregatorResponse' {findingAggregationRegion} -> findingAggregationRegion) (\s@CreateFindingAggregatorResponse' {} a -> s {findingAggregationRegion = a} :: CreateFindingAggregatorResponse)

-- | The ARN of the finding aggregator. You use the finding aggregator ARN to
-- retrieve details for, update, and stop finding aggregation.
createFindingAggregatorResponse_findingAggregatorArn :: Lens.Lens' CreateFindingAggregatorResponse (Prelude.Maybe Prelude.Text)
createFindingAggregatorResponse_findingAggregatorArn = Lens.lens (\CreateFindingAggregatorResponse' {findingAggregatorArn} -> findingAggregatorArn) (\s@CreateFindingAggregatorResponse' {} a -> s {findingAggregatorArn = a} :: CreateFindingAggregatorResponse)

-- | Indicates whether to link all Regions, all Regions except for a list of
-- excluded Regions, or a list of included Regions.
createFindingAggregatorResponse_regionLinkingMode :: Lens.Lens' CreateFindingAggregatorResponse (Prelude.Maybe Prelude.Text)
createFindingAggregatorResponse_regionLinkingMode = Lens.lens (\CreateFindingAggregatorResponse' {regionLinkingMode} -> regionLinkingMode) (\s@CreateFindingAggregatorResponse' {} a -> s {regionLinkingMode = a} :: CreateFindingAggregatorResponse)

-- | The list of excluded Regions or included Regions.
createFindingAggregatorResponse_regions :: Lens.Lens' CreateFindingAggregatorResponse (Prelude.Maybe [Prelude.Text])
createFindingAggregatorResponse_regions = Lens.lens (\CreateFindingAggregatorResponse' {regions} -> regions) (\s@CreateFindingAggregatorResponse' {} a -> s {regions = a} :: CreateFindingAggregatorResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
createFindingAggregatorResponse_httpStatus :: Lens.Lens' CreateFindingAggregatorResponse Prelude.Int
createFindingAggregatorResponse_httpStatus = Lens.lens (\CreateFindingAggregatorResponse' {httpStatus} -> httpStatus) (\s@CreateFindingAggregatorResponse' {} a -> s {httpStatus = a} :: CreateFindingAggregatorResponse)

instance
  Prelude.NFData
    CreateFindingAggregatorResponse
  where
  rnf CreateFindingAggregatorResponse' {..} =
    Prelude.rnf findingAggregationRegion
      `Prelude.seq` Prelude.rnf findingAggregatorArn
      `Prelude.seq` Prelude.rnf regionLinkingMode
      `Prelude.seq` Prelude.rnf regions
      `Prelude.seq` Prelude.rnf httpStatus
