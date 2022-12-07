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
-- Module      : Amazonka.AccessAnalyzer.Types.AccessPreviewStatusReason
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AccessAnalyzer.Types.AccessPreviewStatusReason where

import Amazonka.AccessAnalyzer.Types.AccessPreviewStatusReasonCode
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides more details about the current status of the access preview.
-- For example, if the creation of the access preview fails, a @Failed@
-- status is returned. This failure can be due to an internal issue with
-- the analysis or due to an invalid proposed resource configuration.
--
-- /See:/ 'newAccessPreviewStatusReason' smart constructor.
data AccessPreviewStatusReason = AccessPreviewStatusReason'
  { -- | The reason code for the current status of the access preview.
    code :: AccessPreviewStatusReasonCode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AccessPreviewStatusReason' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'code', 'accessPreviewStatusReason_code' - The reason code for the current status of the access preview.
newAccessPreviewStatusReason ::
  -- | 'code'
  AccessPreviewStatusReasonCode ->
  AccessPreviewStatusReason
newAccessPreviewStatusReason pCode_ =
  AccessPreviewStatusReason' {code = pCode_}

-- | The reason code for the current status of the access preview.
accessPreviewStatusReason_code :: Lens.Lens' AccessPreviewStatusReason AccessPreviewStatusReasonCode
accessPreviewStatusReason_code = Lens.lens (\AccessPreviewStatusReason' {code} -> code) (\s@AccessPreviewStatusReason' {} a -> s {code = a} :: AccessPreviewStatusReason)

instance Data.FromJSON AccessPreviewStatusReason where
  parseJSON =
    Data.withObject
      "AccessPreviewStatusReason"
      ( \x ->
          AccessPreviewStatusReason'
            Prelude.<$> (x Data..: "code")
      )

instance Prelude.Hashable AccessPreviewStatusReason where
  hashWithSalt _salt AccessPreviewStatusReason' {..} =
    _salt `Prelude.hashWithSalt` code

instance Prelude.NFData AccessPreviewStatusReason where
  rnf AccessPreviewStatusReason' {..} = Prelude.rnf code
