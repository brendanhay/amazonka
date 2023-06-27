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
-- Module      : Amazonka.VPCLattice.Types.FixedResponseAction
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.VPCLattice.Types.FixedResponseAction where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about an action that returns a custom HTTP response.
--
-- /See:/ 'newFixedResponseAction' smart constructor.
data FixedResponseAction = FixedResponseAction'
  { -- | The HTTP response code.
    statusCode :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FixedResponseAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'statusCode', 'fixedResponseAction_statusCode' - The HTTP response code.
newFixedResponseAction ::
  -- | 'statusCode'
  Prelude.Natural ->
  FixedResponseAction
newFixedResponseAction pStatusCode_ =
  FixedResponseAction' {statusCode = pStatusCode_}

-- | The HTTP response code.
fixedResponseAction_statusCode :: Lens.Lens' FixedResponseAction Prelude.Natural
fixedResponseAction_statusCode = Lens.lens (\FixedResponseAction' {statusCode} -> statusCode) (\s@FixedResponseAction' {} a -> s {statusCode = a} :: FixedResponseAction)

instance Data.FromJSON FixedResponseAction where
  parseJSON =
    Data.withObject
      "FixedResponseAction"
      ( \x ->
          FixedResponseAction'
            Prelude.<$> (x Data..: "statusCode")
      )

instance Prelude.Hashable FixedResponseAction where
  hashWithSalt _salt FixedResponseAction' {..} =
    _salt `Prelude.hashWithSalt` statusCode

instance Prelude.NFData FixedResponseAction where
  rnf FixedResponseAction' {..} = Prelude.rnf statusCode

instance Data.ToJSON FixedResponseAction where
  toJSON FixedResponseAction' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("statusCode" Data..= statusCode)]
      )
