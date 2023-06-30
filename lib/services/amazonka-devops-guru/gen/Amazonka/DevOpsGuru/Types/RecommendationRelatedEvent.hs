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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DevOpsGuru.Types.RecommendationRelatedEvent where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DevOpsGuru.Types.RecommendationRelatedEventResource
import qualified Amazonka.Prelude as Prelude

-- | Information about an event that is related to a recommendation.
--
-- /See:/ 'newRecommendationRelatedEvent' smart constructor.
data RecommendationRelatedEvent = RecommendationRelatedEvent'
  { -- | The name of the event. This corresponds to the @Name@ field in an
    -- @Event@ object.
    name :: Prelude.Maybe Prelude.Text,
    -- | A @ResourceCollection@ object that contains arrays of the names of
    -- Amazon Web Services CloudFormation stacks. You can specify up to 500
    -- Amazon Web Services CloudFormation stacks.
    resources :: Prelude.Maybe [RecommendationRelatedEventResource]
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
-- 'name', 'recommendationRelatedEvent_name' - The name of the event. This corresponds to the @Name@ field in an
-- @Event@ object.
--
-- 'resources', 'recommendationRelatedEvent_resources' - A @ResourceCollection@ object that contains arrays of the names of
-- Amazon Web Services CloudFormation stacks. You can specify up to 500
-- Amazon Web Services CloudFormation stacks.
newRecommendationRelatedEvent ::
  RecommendationRelatedEvent
newRecommendationRelatedEvent =
  RecommendationRelatedEvent'
    { name = Prelude.Nothing,
      resources = Prelude.Nothing
    }

-- | The name of the event. This corresponds to the @Name@ field in an
-- @Event@ object.
recommendationRelatedEvent_name :: Lens.Lens' RecommendationRelatedEvent (Prelude.Maybe Prelude.Text)
recommendationRelatedEvent_name = Lens.lens (\RecommendationRelatedEvent' {name} -> name) (\s@RecommendationRelatedEvent' {} a -> s {name = a} :: RecommendationRelatedEvent)

-- | A @ResourceCollection@ object that contains arrays of the names of
-- Amazon Web Services CloudFormation stacks. You can specify up to 500
-- Amazon Web Services CloudFormation stacks.
recommendationRelatedEvent_resources :: Lens.Lens' RecommendationRelatedEvent (Prelude.Maybe [RecommendationRelatedEventResource])
recommendationRelatedEvent_resources = Lens.lens (\RecommendationRelatedEvent' {resources} -> resources) (\s@RecommendationRelatedEvent' {} a -> s {resources = a} :: RecommendationRelatedEvent) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON RecommendationRelatedEvent where
  parseJSON =
    Data.withObject
      "RecommendationRelatedEvent"
      ( \x ->
          RecommendationRelatedEvent'
            Prelude.<$> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "Resources" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable RecommendationRelatedEvent where
  hashWithSalt _salt RecommendationRelatedEvent' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` resources

instance Prelude.NFData RecommendationRelatedEvent where
  rnf RecommendationRelatedEvent' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf resources
