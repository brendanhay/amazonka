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
-- Module      : Amazonka.IoT.Types.ViolationEventAdditionalInfo
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.ViolationEventAdditionalInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types.ConfidenceLevel
import qualified Amazonka.Prelude as Prelude

-- | The details of a violation event.
--
-- /See:/ 'newViolationEventAdditionalInfo' smart constructor.
data ViolationEventAdditionalInfo = ViolationEventAdditionalInfo'
  { -- | The sensitivity of anomalous behavior evaluation. Can be @Low@,
    -- @Medium@, or @High@.
    confidenceLevel :: Prelude.Maybe ConfidenceLevel
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ViolationEventAdditionalInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'confidenceLevel', 'violationEventAdditionalInfo_confidenceLevel' - The sensitivity of anomalous behavior evaluation. Can be @Low@,
-- @Medium@, or @High@.
newViolationEventAdditionalInfo ::
  ViolationEventAdditionalInfo
newViolationEventAdditionalInfo =
  ViolationEventAdditionalInfo'
    { confidenceLevel =
        Prelude.Nothing
    }

-- | The sensitivity of anomalous behavior evaluation. Can be @Low@,
-- @Medium@, or @High@.
violationEventAdditionalInfo_confidenceLevel :: Lens.Lens' ViolationEventAdditionalInfo (Prelude.Maybe ConfidenceLevel)
violationEventAdditionalInfo_confidenceLevel = Lens.lens (\ViolationEventAdditionalInfo' {confidenceLevel} -> confidenceLevel) (\s@ViolationEventAdditionalInfo' {} a -> s {confidenceLevel = a} :: ViolationEventAdditionalInfo)

instance Data.FromJSON ViolationEventAdditionalInfo where
  parseJSON =
    Data.withObject
      "ViolationEventAdditionalInfo"
      ( \x ->
          ViolationEventAdditionalInfo'
            Prelude.<$> (x Data..:? "confidenceLevel")
      )

instance
  Prelude.Hashable
    ViolationEventAdditionalInfo
  where
  hashWithSalt _salt ViolationEventAdditionalInfo' {..} =
    _salt `Prelude.hashWithSalt` confidenceLevel

instance Prelude.NFData ViolationEventAdditionalInfo where
  rnf ViolationEventAdditionalInfo' {..} =
    Prelude.rnf confidenceLevel
