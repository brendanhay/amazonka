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
-- Module      : Amazonka.AccessAnalyzer.Types.StatusReason
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AccessAnalyzer.Types.StatusReason where

import Amazonka.AccessAnalyzer.Types.ReasonCode
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides more details about the current status of the analyzer. For
-- example, if the creation for the analyzer fails, a @Failed@ status is
-- returned. For an analyzer with organization as the type, this failure
-- can be due to an issue with creating the service-linked roles required
-- in the member accounts of the Amazon Web Services organization.
--
-- /See:/ 'newStatusReason' smart constructor.
data StatusReason = StatusReason'
  { -- | The reason code for the current status of the analyzer.
    code :: ReasonCode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StatusReason' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'code', 'statusReason_code' - The reason code for the current status of the analyzer.
newStatusReason ::
  -- | 'code'
  ReasonCode ->
  StatusReason
newStatusReason pCode_ = StatusReason' {code = pCode_}

-- | The reason code for the current status of the analyzer.
statusReason_code :: Lens.Lens' StatusReason ReasonCode
statusReason_code = Lens.lens (\StatusReason' {code} -> code) (\s@StatusReason' {} a -> s {code = a} :: StatusReason)

instance Data.FromJSON StatusReason where
  parseJSON =
    Data.withObject
      "StatusReason"
      (\x -> StatusReason' Prelude.<$> (x Data..: "code"))

instance Prelude.Hashable StatusReason where
  hashWithSalt _salt StatusReason' {..} =
    _salt `Prelude.hashWithSalt` code

instance Prelude.NFData StatusReason where
  rnf StatusReason' {..} = Prelude.rnf code
