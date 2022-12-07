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
-- Module      : Amazonka.SecurityHub.Types.StandardsStatusReason
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.StandardsStatusReason where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.StatusReasonCode

-- | The reason for the current status of a standard subscription.
--
-- /See:/ 'newStandardsStatusReason' smart constructor.
data StandardsStatusReason = StandardsStatusReason'
  { -- | The reason code that represents the reason for the current status of a
    -- standard subscription.
    statusReasonCode :: StatusReasonCode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StandardsStatusReason' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'statusReasonCode', 'standardsStatusReason_statusReasonCode' - The reason code that represents the reason for the current status of a
-- standard subscription.
newStandardsStatusReason ::
  -- | 'statusReasonCode'
  StatusReasonCode ->
  StandardsStatusReason
newStandardsStatusReason pStatusReasonCode_ =
  StandardsStatusReason'
    { statusReasonCode =
        pStatusReasonCode_
    }

-- | The reason code that represents the reason for the current status of a
-- standard subscription.
standardsStatusReason_statusReasonCode :: Lens.Lens' StandardsStatusReason StatusReasonCode
standardsStatusReason_statusReasonCode = Lens.lens (\StandardsStatusReason' {statusReasonCode} -> statusReasonCode) (\s@StandardsStatusReason' {} a -> s {statusReasonCode = a} :: StandardsStatusReason)

instance Data.FromJSON StandardsStatusReason where
  parseJSON =
    Data.withObject
      "StandardsStatusReason"
      ( \x ->
          StandardsStatusReason'
            Prelude.<$> (x Data..: "StatusReasonCode")
      )

instance Prelude.Hashable StandardsStatusReason where
  hashWithSalt _salt StandardsStatusReason' {..} =
    _salt `Prelude.hashWithSalt` statusReasonCode

instance Prelude.NFData StandardsStatusReason where
  rnf StandardsStatusReason' {..} =
    Prelude.rnf statusReasonCode
