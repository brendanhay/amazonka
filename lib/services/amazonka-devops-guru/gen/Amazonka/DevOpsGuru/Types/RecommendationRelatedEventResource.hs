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
-- Module      : Amazonka.DevOpsGuru.Types.RecommendationRelatedEventResource
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DevOpsGuru.Types.RecommendationRelatedEventResource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Information about an Amazon Web Services resource that emitted and event
-- that is related to a recommendation in an insight.
--
-- /See:/ 'newRecommendationRelatedEventResource' smart constructor.
data RecommendationRelatedEventResource = RecommendationRelatedEventResource'
  { -- | The name of the resource that emitted the event. This corresponds to the
    -- @Name@ field in an @EventResource@ object.
    name :: Prelude.Maybe Prelude.Text,
    -- | The type of the resource that emitted the event. This corresponds to the
    -- @Type@ field in an @EventResource@ object.
    type' :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RecommendationRelatedEventResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'recommendationRelatedEventResource_name' - The name of the resource that emitted the event. This corresponds to the
-- @Name@ field in an @EventResource@ object.
--
-- 'type'', 'recommendationRelatedEventResource_type' - The type of the resource that emitted the event. This corresponds to the
-- @Type@ field in an @EventResource@ object.
newRecommendationRelatedEventResource ::
  RecommendationRelatedEventResource
newRecommendationRelatedEventResource =
  RecommendationRelatedEventResource'
    { name =
        Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The name of the resource that emitted the event. This corresponds to the
-- @Name@ field in an @EventResource@ object.
recommendationRelatedEventResource_name :: Lens.Lens' RecommendationRelatedEventResource (Prelude.Maybe Prelude.Text)
recommendationRelatedEventResource_name = Lens.lens (\RecommendationRelatedEventResource' {name} -> name) (\s@RecommendationRelatedEventResource' {} a -> s {name = a} :: RecommendationRelatedEventResource)

-- | The type of the resource that emitted the event. This corresponds to the
-- @Type@ field in an @EventResource@ object.
recommendationRelatedEventResource_type :: Lens.Lens' RecommendationRelatedEventResource (Prelude.Maybe Prelude.Text)
recommendationRelatedEventResource_type = Lens.lens (\RecommendationRelatedEventResource' {type'} -> type') (\s@RecommendationRelatedEventResource' {} a -> s {type' = a} :: RecommendationRelatedEventResource)

instance
  Core.FromJSON
    RecommendationRelatedEventResource
  where
  parseJSON =
    Core.withObject
      "RecommendationRelatedEventResource"
      ( \x ->
          RecommendationRelatedEventResource'
            Prelude.<$> (x Core..:? "Name") Prelude.<*> (x Core..:? "Type")
      )

instance
  Prelude.Hashable
    RecommendationRelatedEventResource
  where
  hashWithSalt
    _salt
    RecommendationRelatedEventResource' {..} =
      _salt `Prelude.hashWithSalt` name
        `Prelude.hashWithSalt` type'

instance
  Prelude.NFData
    RecommendationRelatedEventResource
  where
  rnf RecommendationRelatedEventResource' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf type'
