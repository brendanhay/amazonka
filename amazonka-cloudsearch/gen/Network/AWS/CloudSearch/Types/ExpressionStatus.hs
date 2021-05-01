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
-- Module      : Network.AWS.CloudSearch.Types.ExpressionStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudSearch.Types.ExpressionStatus where

import Network.AWS.CloudSearch.Types.Expression
import Network.AWS.CloudSearch.Types.OptionStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The value of an @Expression@ and its current status.
--
-- /See:/ 'newExpressionStatus' smart constructor.
data ExpressionStatus = ExpressionStatus'
  { -- | The expression that is evaluated for sorting while processing a search
    -- request.
    options :: Expression,
    status :: OptionStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ExpressionStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'options', 'expressionStatus_options' - The expression that is evaluated for sorting while processing a search
-- request.
--
-- 'status', 'expressionStatus_status' - Undocumented member.
newExpressionStatus ::
  -- | 'options'
  Expression ->
  -- | 'status'
  OptionStatus ->
  ExpressionStatus
newExpressionStatus pOptions_ pStatus_ =
  ExpressionStatus'
    { options = pOptions_,
      status = pStatus_
    }

-- | The expression that is evaluated for sorting while processing a search
-- request.
expressionStatus_options :: Lens.Lens' ExpressionStatus Expression
expressionStatus_options = Lens.lens (\ExpressionStatus' {options} -> options) (\s@ExpressionStatus' {} a -> s {options = a} :: ExpressionStatus)

-- | Undocumented member.
expressionStatus_status :: Lens.Lens' ExpressionStatus OptionStatus
expressionStatus_status = Lens.lens (\ExpressionStatus' {status} -> status) (\s@ExpressionStatus' {} a -> s {status = a} :: ExpressionStatus)

instance Prelude.FromXML ExpressionStatus where
  parseXML x =
    ExpressionStatus'
      Prelude.<$> (x Prelude..@ "Options")
      Prelude.<*> (x Prelude..@ "Status")

instance Prelude.Hashable ExpressionStatus

instance Prelude.NFData ExpressionStatus
