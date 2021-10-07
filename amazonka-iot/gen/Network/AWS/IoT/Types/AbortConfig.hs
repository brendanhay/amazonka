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
import qualified Network.AWS.Prelude as Prelude

-- | The criteria that determine when and how a job abort takes place.
--
-- /See:/ 'newAbortConfig' smart constructor.
data AbortConfig = AbortConfig'
  { -- | The list of criteria that determine when and how to abort the job.
    criteriaList :: Prelude.NonEmpty AbortCriteria
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.NonEmpty AbortCriteria ->
  AbortConfig
newAbortConfig pCriteriaList_ =
  AbortConfig'
    { criteriaList =
        Lens._Coerce Lens.# pCriteriaList_
    }

-- | The list of criteria that determine when and how to abort the job.
abortConfig_criteriaList :: Lens.Lens' AbortConfig (Prelude.NonEmpty AbortCriteria)
abortConfig_criteriaList = Lens.lens (\AbortConfig' {criteriaList} -> criteriaList) (\s@AbortConfig' {} a -> s {criteriaList = a} :: AbortConfig) Prelude.. Lens._Coerce

instance Core.FromJSON AbortConfig where
  parseJSON =
    Core.withObject
      "AbortConfig"
      ( \x ->
          AbortConfig' Prelude.<$> (x Core..: "criteriaList")
      )

instance Prelude.Hashable AbortConfig

instance Prelude.NFData AbortConfig

instance Core.ToJSON AbortConfig where
  toJSON AbortConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("criteriaList" Core..= criteriaList)]
      )
