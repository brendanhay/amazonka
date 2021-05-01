{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.IoT.Types.ViolationEventAdditionalInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.ViolationEventAdditionalInfo where

import Network.AWS.IoT.Types.ConfidenceLevel
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The details of a violation event.
--
-- /See:/ 'newViolationEventAdditionalInfo' smart constructor.
data ViolationEventAdditionalInfo = ViolationEventAdditionalInfo'
  { -- | The sensitivity of anomalous behavior evaluation. Can be @Low@,
    -- @Medium@, or @High@.
    confidenceLevel :: Prelude.Maybe ConfidenceLevel
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance
  Prelude.FromJSON
    ViolationEventAdditionalInfo
  where
  parseJSON =
    Prelude.withObject
      "ViolationEventAdditionalInfo"
      ( \x ->
          ViolationEventAdditionalInfo'
            Prelude.<$> (x Prelude..:? "confidenceLevel")
      )

instance
  Prelude.Hashable
    ViolationEventAdditionalInfo

instance Prelude.NFData ViolationEventAdditionalInfo
