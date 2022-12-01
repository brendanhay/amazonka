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
-- Module      : Amazonka.MediaConvert.Types.OutputGroupDetail
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.OutputGroupDetail where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MediaConvert.Types.OutputDetail
import qualified Amazonka.Prelude as Prelude

-- | Contains details about the output groups specified in the job settings.
--
-- /See:/ 'newOutputGroupDetail' smart constructor.
data OutputGroupDetail = OutputGroupDetail'
  { -- | Details about the output
    outputDetails :: Prelude.Maybe [OutputDetail]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OutputGroupDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'outputDetails', 'outputGroupDetail_outputDetails' - Details about the output
newOutputGroupDetail ::
  OutputGroupDetail
newOutputGroupDetail =
  OutputGroupDetail' {outputDetails = Prelude.Nothing}

-- | Details about the output
outputGroupDetail_outputDetails :: Lens.Lens' OutputGroupDetail (Prelude.Maybe [OutputDetail])
outputGroupDetail_outputDetails = Lens.lens (\OutputGroupDetail' {outputDetails} -> outputDetails) (\s@OutputGroupDetail' {} a -> s {outputDetails = a} :: OutputGroupDetail) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON OutputGroupDetail where
  parseJSON =
    Core.withObject
      "OutputGroupDetail"
      ( \x ->
          OutputGroupDetail'
            Prelude.<$> (x Core..:? "outputDetails" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable OutputGroupDetail where
  hashWithSalt _salt OutputGroupDetail' {..} =
    _salt `Prelude.hashWithSalt` outputDetails

instance Prelude.NFData OutputGroupDetail where
  rnf OutputGroupDetail' {..} =
    Prelude.rnf outputDetails
