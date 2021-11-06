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
-- Module      : Amazonka.DevOpsGuru.Types.RecommendationRelatedEvent
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DevOpsGuru.Types.RecommendationRelatedEvent where

import qualified Amazonka.Core as Core
import Amazonka.DevOpsGuru.Types.RecommendationRelatedEventResource
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Information about an event that is related to a recommendation.
--
-- /See:/ 'newRecommendationRelatedEvent' smart constructor.
data RecommendationRelatedEvent = RecommendationRelatedEvent'
  { -- | A @ResourceCollection@ object that contains arrays of the names of AWS
    -- CloudFormation stacks. You can specify up to 500 AWS CloudFormation
    -- stacks.
    resources :: Prelude.Maybe [RecommendationRelatedEventResource],
    -- | The name of the event. This corresponds to the @Name@ field in an
    -- @Event@ object.
    name :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RecommendationRelatedEvent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resources', 'recommendationRelatedEvent_resources' - A @ResourceCollection@ object that contains arrays of the names of AWS
-- CloudFormation stacks. You can specify up to 500 AWS CloudFormation
-- stacks.
--
-- 'name', 'recommendationRelatedEvent_name' - The name of the event. This corresponds to the @Name@ field in an
-- @Event@ object.
newRecommendationRelatedEvent ::
  RecommendationRelatedEvent
newRecommendationRelatedEvent =
  RecommendationRelatedEvent'
    { resources =
        Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | A @ResourceCollection@ object that contains arrays of the names of AWS
-- CloudFormation stacks. You can specify up to 500 AWS CloudFormation
-- stacks.
recommendationRelatedEvent_resources :: Lens.Lens' RecommendationRelatedEvent (Prelude.Maybe [RecommendationRelatedEventResource])
recommendationRelatedEvent_resources = Lens.lens (\RecommendationRelatedEvent' {resources} -> resources) (\s@RecommendationRelatedEvent' {} a -> s {resources = a} :: RecommendationRelatedEvent) Prelude.. Lens.mapping Lens.coerced

-- | The name of the event. This corresponds to the @Name@ field in an
-- @Event@ object.
recommendationRelatedEvent_name :: Lens.Lens' RecommendationRelatedEvent (Prelude.Maybe Prelude.Text)
recommendationRelatedEvent_name = Lens.lens (\RecommendationRelatedEvent' {name} -> name) (\s@RecommendationRelatedEvent' {} a -> s {name = a} :: RecommendationRelatedEvent)

instance Core.FromJSON RecommendationRelatedEvent where
  parseJSON =
    Core.withObject
      "RecommendationRelatedEvent"
      ( \x ->
          RecommendationRelatedEvent'
            Prelude.<$> (x Core..:? "Resources" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "Name")
      )

instance Prelude.Hashable RecommendationRelatedEvent

instance Prelude.NFData RecommendationRelatedEvent
