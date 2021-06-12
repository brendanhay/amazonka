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
-- Module      : Network.AWS.IoT.Types.AbortConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.AbortConfig where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types.AbortCriteria
import qualified Network.AWS.Lens as Lens

-- | The criteria that determine when and how a job abort takes place.
--
-- /See:/ 'newAbortConfig' smart constructor.
data AbortConfig = AbortConfig'
  { -- | The list of criteria that determine when and how to abort the job.
    criteriaList :: Core.NonEmpty AbortCriteria
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AbortConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'criteriaList', 'abortConfig_criteriaList' - The list of criteria that determine when and how to abort the job.
newAbortConfig ::
  -- | 'criteriaList'
  Core.NonEmpty AbortCriteria ->
  AbortConfig
newAbortConfig pCriteriaList_ =
  AbortConfig'
    { criteriaList =
        Lens._Coerce Lens.# pCriteriaList_
    }

-- | The list of criteria that determine when and how to abort the job.
abortConfig_criteriaList :: Lens.Lens' AbortConfig (Core.NonEmpty AbortCriteria)
abortConfig_criteriaList = Lens.lens (\AbortConfig' {criteriaList} -> criteriaList) (\s@AbortConfig' {} a -> s {criteriaList = a} :: AbortConfig) Core.. Lens._Coerce

instance Core.FromJSON AbortConfig where
  parseJSON =
    Core.withObject
      "AbortConfig"
      ( \x ->
          AbortConfig' Core.<$> (x Core..: "criteriaList")
      )

instance Core.Hashable AbortConfig

instance Core.NFData AbortConfig

instance Core.ToJSON AbortConfig where
  toJSON AbortConfig' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("criteriaList" Core..= criteriaList)]
      )
