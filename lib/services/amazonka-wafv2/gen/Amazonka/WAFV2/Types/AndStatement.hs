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
-- Module      : Amazonka.WAFV2.Types.AndStatement
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WAFV2.Types.AndStatement where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import {-# SOURCE #-} Amazonka.WAFV2.Types.Statement

-- | A logical rule statement used to combine other rule statements with AND
-- logic. You provide more than one Statement within the @AndStatement@.
--
-- /See:/ 'newAndStatement' smart constructor.
data AndStatement = AndStatement'
  { -- | The statements to combine with AND logic. You can use any statements
    -- that can be nested.
    statements :: [Statement]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AndStatement' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'statements', 'andStatement_statements' - The statements to combine with AND logic. You can use any statements
-- that can be nested.
newAndStatement ::
  AndStatement
newAndStatement =
  AndStatement' {statements = Prelude.mempty}

-- | The statements to combine with AND logic. You can use any statements
-- that can be nested.
andStatement_statements :: Lens.Lens' AndStatement [Statement]
andStatement_statements = Lens.lens (\AndStatement' {statements} -> statements) (\s@AndStatement' {} a -> s {statements = a} :: AndStatement) Prelude.. Lens.coerced

instance Data.FromJSON AndStatement where
  parseJSON =
    Data.withObject
      "AndStatement"
      ( \x ->
          AndStatement'
            Prelude.<$> (x Data..:? "Statements" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable AndStatement where
  hashWithSalt _salt AndStatement' {..} =
    _salt `Prelude.hashWithSalt` statements

instance Prelude.NFData AndStatement where
  rnf AndStatement' {..} = Prelude.rnf statements

instance Data.ToJSON AndStatement where
  toJSON AndStatement' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Statements" Data..= statements)]
      )
