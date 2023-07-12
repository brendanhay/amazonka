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
-- Module      : Amazonka.Amplify.Types.CustomRule
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Amplify.Types.CustomRule where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes a custom rewrite or redirect rule.
--
-- /See:/ 'newCustomRule' smart constructor.
data CustomRule = CustomRule'
  { -- | The condition for a URL rewrite or redirect rule, such as a country
    -- code.
    condition :: Prelude.Maybe Prelude.Text,
    -- | The status code for a URL rewrite or redirect rule.
    --
    -- [200]
    --     Represents a 200 rewrite rule.
    --
    -- [301]
    --     Represents a 301 (moved pemanently) redirect rule. This and all
    --     future requests should be directed to the target URL.
    --
    -- [302]
    --     Represents a 302 temporary redirect rule.
    --
    -- [404]
    --     Represents a 404 redirect rule.
    --
    -- [404-200]
    --     Represents a 404 rewrite rule.
    status :: Prelude.Maybe Prelude.Text,
    -- | The source pattern for a URL rewrite or redirect rule.
    source :: Prelude.Text,
    -- | The target pattern for a URL rewrite or redirect rule.
    target :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CustomRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'condition', 'customRule_condition' - The condition for a URL rewrite or redirect rule, such as a country
-- code.
--
-- 'status', 'customRule_status' - The status code for a URL rewrite or redirect rule.
--
-- [200]
--     Represents a 200 rewrite rule.
--
-- [301]
--     Represents a 301 (moved pemanently) redirect rule. This and all
--     future requests should be directed to the target URL.
--
-- [302]
--     Represents a 302 temporary redirect rule.
--
-- [404]
--     Represents a 404 redirect rule.
--
-- [404-200]
--     Represents a 404 rewrite rule.
--
-- 'source', 'customRule_source' - The source pattern for a URL rewrite or redirect rule.
--
-- 'target', 'customRule_target' - The target pattern for a URL rewrite or redirect rule.
newCustomRule ::
  -- | 'source'
  Prelude.Text ->
  -- | 'target'
  Prelude.Text ->
  CustomRule
newCustomRule pSource_ pTarget_ =
  CustomRule'
    { condition = Prelude.Nothing,
      status = Prelude.Nothing,
      source = pSource_,
      target = pTarget_
    }

-- | The condition for a URL rewrite or redirect rule, such as a country
-- code.
customRule_condition :: Lens.Lens' CustomRule (Prelude.Maybe Prelude.Text)
customRule_condition = Lens.lens (\CustomRule' {condition} -> condition) (\s@CustomRule' {} a -> s {condition = a} :: CustomRule)

-- | The status code for a URL rewrite or redirect rule.
--
-- [200]
--     Represents a 200 rewrite rule.
--
-- [301]
--     Represents a 301 (moved pemanently) redirect rule. This and all
--     future requests should be directed to the target URL.
--
-- [302]
--     Represents a 302 temporary redirect rule.
--
-- [404]
--     Represents a 404 redirect rule.
--
-- [404-200]
--     Represents a 404 rewrite rule.
customRule_status :: Lens.Lens' CustomRule (Prelude.Maybe Prelude.Text)
customRule_status = Lens.lens (\CustomRule' {status} -> status) (\s@CustomRule' {} a -> s {status = a} :: CustomRule)

-- | The source pattern for a URL rewrite or redirect rule.
customRule_source :: Lens.Lens' CustomRule Prelude.Text
customRule_source = Lens.lens (\CustomRule' {source} -> source) (\s@CustomRule' {} a -> s {source = a} :: CustomRule)

-- | The target pattern for a URL rewrite or redirect rule.
customRule_target :: Lens.Lens' CustomRule Prelude.Text
customRule_target = Lens.lens (\CustomRule' {target} -> target) (\s@CustomRule' {} a -> s {target = a} :: CustomRule)

instance Data.FromJSON CustomRule where
  parseJSON =
    Data.withObject
      "CustomRule"
      ( \x ->
          CustomRule'
            Prelude.<$> (x Data..:? "condition")
            Prelude.<*> (x Data..:? "status")
            Prelude.<*> (x Data..: "source")
            Prelude.<*> (x Data..: "target")
      )

instance Prelude.Hashable CustomRule where
  hashWithSalt _salt CustomRule' {..} =
    _salt
      `Prelude.hashWithSalt` condition
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` source
      `Prelude.hashWithSalt` target

instance Prelude.NFData CustomRule where
  rnf CustomRule' {..} =
    Prelude.rnf condition
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf source
      `Prelude.seq` Prelude.rnf target

instance Data.ToJSON CustomRule where
  toJSON CustomRule' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("condition" Data..=) Prelude.<$> condition,
            ("status" Data..=) Prelude.<$> status,
            Prelude.Just ("source" Data..= source),
            Prelude.Just ("target" Data..= target)
          ]
      )
