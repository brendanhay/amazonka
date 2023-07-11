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
-- Module      : Amazonka.WAFV2.Types.OrStatement
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WAFV2.Types.OrStatement where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import {-# SOURCE #-} Amazonka.WAFV2.Types.Statement

-- | A logical rule statement used to combine other rule statements with OR
-- logic. You provide more than one Statement within the @OrStatement@.
--
-- /See:/ 'newOrStatement' smart constructor.
data OrStatement = OrStatement'
  { -- | The statements to combine with OR logic. You can use any statements that
    -- can be nested.
    statements :: [Statement]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OrStatement' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'statements', 'orStatement_statements' - The statements to combine with OR logic. You can use any statements that
-- can be nested.
newOrStatement ::
  OrStatement
newOrStatement =
  OrStatement' {statements = Prelude.mempty}

-- | The statements to combine with OR logic. You can use any statements that
-- can be nested.
orStatement_statements :: Lens.Lens' OrStatement [Statement]
orStatement_statements = Lens.lens (\OrStatement' {statements} -> statements) (\s@OrStatement' {} a -> s {statements = a} :: OrStatement) Prelude.. Lens.coerced

instance Data.FromJSON OrStatement where
  parseJSON =
    Data.withObject
      "OrStatement"
      ( \x ->
          OrStatement'
            Prelude.<$> (x Data..:? "Statements" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable OrStatement where
  hashWithSalt _salt OrStatement' {..} =
    _salt `Prelude.hashWithSalt` statements

instance Prelude.NFData OrStatement where
  rnf OrStatement' {..} = Prelude.rnf statements

instance Data.ToJSON OrStatement where
  toJSON OrStatement' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Statements" Data..= statements)]
      )
