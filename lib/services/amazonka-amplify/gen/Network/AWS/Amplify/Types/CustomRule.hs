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
-- Module      : Network.AWS.Amplify.Types.CustomRule
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Amplify.Types.CustomRule where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes a custom rewrite or redirect rule.
--
-- /See:/ 'newCustomRule' smart constructor.
data CustomRule = CustomRule'
  { -- | The status code for a URL rewrite or redirect rule.
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
    -- | The condition for a URL rewrite or redirect rule, such as a country
    -- code.
    condition :: Prelude.Maybe Prelude.Text,
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
-- 'condition', 'customRule_condition' - The condition for a URL rewrite or redirect rule, such as a country
-- code.
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
    { status = Prelude.Nothing,
      condition = Prelude.Nothing,
      source = pSource_,
      target = pTarget_
    }

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

-- | The condition for a URL rewrite or redirect rule, such as a country
-- code.
customRule_condition :: Lens.Lens' CustomRule (Prelude.Maybe Prelude.Text)
customRule_condition = Lens.lens (\CustomRule' {condition} -> condition) (\s@CustomRule' {} a -> s {condition = a} :: CustomRule)

-- | The source pattern for a URL rewrite or redirect rule.
customRule_source :: Lens.Lens' CustomRule Prelude.Text
customRule_source = Lens.lens (\CustomRule' {source} -> source) (\s@CustomRule' {} a -> s {source = a} :: CustomRule)

-- | The target pattern for a URL rewrite or redirect rule.
customRule_target :: Lens.Lens' CustomRule Prelude.Text
customRule_target = Lens.lens (\CustomRule' {target} -> target) (\s@CustomRule' {} a -> s {target = a} :: CustomRule)

instance Core.FromJSON CustomRule where
  parseJSON =
    Core.withObject
      "CustomRule"
      ( \x ->
          CustomRule'
            Prelude.<$> (x Core..:? "status")
            Prelude.<*> (x Core..:? "condition")
            Prelude.<*> (x Core..: "source")
            Prelude.<*> (x Core..: "target")
      )

instance Prelude.Hashable CustomRule

instance Prelude.NFData CustomRule

instance Core.ToJSON CustomRule where
  toJSON CustomRule' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("status" Core..=) Prelude.<$> status,
            ("condition" Core..=) Prelude.<$> condition,
            Prelude.Just ("source" Core..= source),
            Prelude.Just ("target" Core..= target)
          ]
      )
