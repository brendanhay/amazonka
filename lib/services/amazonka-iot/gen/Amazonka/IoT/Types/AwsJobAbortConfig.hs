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
-- Module      : Amazonka.IoT.Types.AwsJobAbortConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.AwsJobAbortConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoT.Types.AwsJobAbortCriteria
import qualified Amazonka.Prelude as Prelude

-- | The criteria that determine when and how a job abort takes place.
--
-- /See:/ 'newAwsJobAbortConfig' smart constructor.
data AwsJobAbortConfig = AwsJobAbortConfig'
  { -- | The list of criteria that determine when and how to abort the job.
    abortCriteriaList :: Prelude.NonEmpty AwsJobAbortCriteria
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsJobAbortConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'abortCriteriaList', 'awsJobAbortConfig_abortCriteriaList' - The list of criteria that determine when and how to abort the job.
newAwsJobAbortConfig ::
  -- | 'abortCriteriaList'
  Prelude.NonEmpty AwsJobAbortCriteria ->
  AwsJobAbortConfig
newAwsJobAbortConfig pAbortCriteriaList_ =
  AwsJobAbortConfig'
    { abortCriteriaList =
        Lens.coerced Lens.# pAbortCriteriaList_
    }

-- | The list of criteria that determine when and how to abort the job.
awsJobAbortConfig_abortCriteriaList :: Lens.Lens' AwsJobAbortConfig (Prelude.NonEmpty AwsJobAbortCriteria)
awsJobAbortConfig_abortCriteriaList = Lens.lens (\AwsJobAbortConfig' {abortCriteriaList} -> abortCriteriaList) (\s@AwsJobAbortConfig' {} a -> s {abortCriteriaList = a} :: AwsJobAbortConfig) Prelude.. Lens.coerced

instance Prelude.Hashable AwsJobAbortConfig where
  hashWithSalt _salt AwsJobAbortConfig' {..} =
    _salt `Prelude.hashWithSalt` abortCriteriaList

instance Prelude.NFData AwsJobAbortConfig where
  rnf AwsJobAbortConfig' {..} =
    Prelude.rnf abortCriteriaList

instance Core.ToJSON AwsJobAbortConfig where
  toJSON AwsJobAbortConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("abortCriteriaList" Core..= abortCriteriaList)
          ]
      )
