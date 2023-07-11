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
-- Module      : Amazonka.CodePipeline.Types.ActionTypeArtifactDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodePipeline.Types.ActionTypeArtifactDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about parameters for artifacts associated with the action
-- type, such as the minimum and maximum artifacts allowed.
--
-- /See:/ 'newActionTypeArtifactDetails' smart constructor.
data ActionTypeArtifactDetails = ActionTypeArtifactDetails'
  { -- | The minimum number of artifacts that can be used with the action type.
    -- For example, you should specify a minimum and maximum of zero input
    -- artifacts for an action type with a category of @source@.
    minimumCount :: Prelude.Natural,
    -- | The maximum number of artifacts that can be used with the actiontype.
    -- For example, you should specify a minimum and maximum of zero input
    -- artifacts for an action type with a category of @source@.
    maximumCount :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ActionTypeArtifactDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'minimumCount', 'actionTypeArtifactDetails_minimumCount' - The minimum number of artifacts that can be used with the action type.
-- For example, you should specify a minimum and maximum of zero input
-- artifacts for an action type with a category of @source@.
--
-- 'maximumCount', 'actionTypeArtifactDetails_maximumCount' - The maximum number of artifacts that can be used with the actiontype.
-- For example, you should specify a minimum and maximum of zero input
-- artifacts for an action type with a category of @source@.
newActionTypeArtifactDetails ::
  -- | 'minimumCount'
  Prelude.Natural ->
  -- | 'maximumCount'
  Prelude.Natural ->
  ActionTypeArtifactDetails
newActionTypeArtifactDetails
  pMinimumCount_
  pMaximumCount_ =
    ActionTypeArtifactDetails'
      { minimumCount =
          pMinimumCount_,
        maximumCount = pMaximumCount_
      }

-- | The minimum number of artifacts that can be used with the action type.
-- For example, you should specify a minimum and maximum of zero input
-- artifacts for an action type with a category of @source@.
actionTypeArtifactDetails_minimumCount :: Lens.Lens' ActionTypeArtifactDetails Prelude.Natural
actionTypeArtifactDetails_minimumCount = Lens.lens (\ActionTypeArtifactDetails' {minimumCount} -> minimumCount) (\s@ActionTypeArtifactDetails' {} a -> s {minimumCount = a} :: ActionTypeArtifactDetails)

-- | The maximum number of artifacts that can be used with the actiontype.
-- For example, you should specify a minimum and maximum of zero input
-- artifacts for an action type with a category of @source@.
actionTypeArtifactDetails_maximumCount :: Lens.Lens' ActionTypeArtifactDetails Prelude.Natural
actionTypeArtifactDetails_maximumCount = Lens.lens (\ActionTypeArtifactDetails' {maximumCount} -> maximumCount) (\s@ActionTypeArtifactDetails' {} a -> s {maximumCount = a} :: ActionTypeArtifactDetails)

instance Data.FromJSON ActionTypeArtifactDetails where
  parseJSON =
    Data.withObject
      "ActionTypeArtifactDetails"
      ( \x ->
          ActionTypeArtifactDetails'
            Prelude.<$> (x Data..: "minimumCount")
            Prelude.<*> (x Data..: "maximumCount")
      )

instance Prelude.Hashable ActionTypeArtifactDetails where
  hashWithSalt _salt ActionTypeArtifactDetails' {..} =
    _salt
      `Prelude.hashWithSalt` minimumCount
      `Prelude.hashWithSalt` maximumCount

instance Prelude.NFData ActionTypeArtifactDetails where
  rnf ActionTypeArtifactDetails' {..} =
    Prelude.rnf minimumCount
      `Prelude.seq` Prelude.rnf maximumCount

instance Data.ToJSON ActionTypeArtifactDetails where
  toJSON ActionTypeArtifactDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("minimumCount" Data..= minimumCount),
            Prelude.Just ("maximumCount" Data..= maximumCount)
          ]
      )
