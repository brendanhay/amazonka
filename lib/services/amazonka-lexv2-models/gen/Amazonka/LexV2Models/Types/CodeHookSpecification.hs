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
-- Module      : Amazonka.LexV2Models.Types.CodeHookSpecification
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.CodeHookSpecification where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types.LambdaCodeHook
import qualified Amazonka.Prelude as Prelude

-- | Contains information about code hooks that Amazon Lex calls during a
-- conversation.
--
-- /See:/ 'newCodeHookSpecification' smart constructor.
data CodeHookSpecification = CodeHookSpecification'
  { lambdaCodeHook :: LambdaCodeHook
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CodeHookSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lambdaCodeHook', 'codeHookSpecification_lambdaCodeHook' - Undocumented member.
newCodeHookSpecification ::
  -- | 'lambdaCodeHook'
  LambdaCodeHook ->
  CodeHookSpecification
newCodeHookSpecification pLambdaCodeHook_ =
  CodeHookSpecification'
    { lambdaCodeHook =
        pLambdaCodeHook_
    }

-- | Undocumented member.
codeHookSpecification_lambdaCodeHook :: Lens.Lens' CodeHookSpecification LambdaCodeHook
codeHookSpecification_lambdaCodeHook = Lens.lens (\CodeHookSpecification' {lambdaCodeHook} -> lambdaCodeHook) (\s@CodeHookSpecification' {} a -> s {lambdaCodeHook = a} :: CodeHookSpecification)

instance Data.FromJSON CodeHookSpecification where
  parseJSON =
    Data.withObject
      "CodeHookSpecification"
      ( \x ->
          CodeHookSpecification'
            Prelude.<$> (x Data..: "lambdaCodeHook")
      )

instance Prelude.Hashable CodeHookSpecification where
  hashWithSalt _salt CodeHookSpecification' {..} =
    _salt `Prelude.hashWithSalt` lambdaCodeHook

instance Prelude.NFData CodeHookSpecification where
  rnf CodeHookSpecification' {..} =
    Prelude.rnf lambdaCodeHook

instance Data.ToJSON CodeHookSpecification where
  toJSON CodeHookSpecification' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("lambdaCodeHook" Data..= lambdaCodeHook)
          ]
      )
