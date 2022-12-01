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
-- Module      : Amazonka.WAFV2.Types.NotStatement
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WAFV2.Types.NotStatement where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import {-# SOURCE #-} Amazonka.WAFV2.Types.Statement

-- | A logical rule statement used to negate the results of another rule
-- statement. You provide one Statement within the @NotStatement@.
--
-- /See:/ 'newNotStatement' smart constructor.
data NotStatement = NotStatement'
  { -- | The statement to negate. You can use any statement that can be nested.
    statement :: Statement
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NotStatement' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'statement', 'notStatement_statement' - The statement to negate. You can use any statement that can be nested.
newNotStatement ::
  -- | 'statement'
  Statement ->
  NotStatement
newNotStatement pStatement_ =
  NotStatement' {statement = pStatement_}

-- | The statement to negate. You can use any statement that can be nested.
notStatement_statement :: Lens.Lens' NotStatement Statement
notStatement_statement = Lens.lens (\NotStatement' {statement} -> statement) (\s@NotStatement' {} a -> s {statement = a} :: NotStatement)

instance Core.FromJSON NotStatement where
  parseJSON =
    Core.withObject
      "NotStatement"
      ( \x ->
          NotStatement' Prelude.<$> (x Core..: "Statement")
      )

instance Prelude.Hashable NotStatement where
  hashWithSalt _salt NotStatement' {..} =
    _salt `Prelude.hashWithSalt` statement

instance Prelude.NFData NotStatement where
  rnf NotStatement' {..} = Prelude.rnf statement

instance Core.ToJSON NotStatement where
  toJSON NotStatement' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("Statement" Core..= statement)]
      )
