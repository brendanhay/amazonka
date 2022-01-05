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
-- Module      : Amazonka.S3.Types.LifecycleRuleAndOperator
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.S3.Types.LifecycleRuleAndOperator where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.S3.Internal
import Amazonka.S3.Types.Tag

-- | This is used in a Lifecycle Rule Filter to apply a logical AND to two or
-- more predicates. The Lifecycle Rule will apply to any object matching
-- all of the predicates configured inside the And operator.
--
-- /See:/ 'newLifecycleRuleAndOperator' smart constructor.
data LifecycleRuleAndOperator = LifecycleRuleAndOperator'
  { -- | Prefix identifying one or more objects to which the rule applies.
    prefix :: Prelude.Maybe Prelude.Text,
    -- | All of these tags must exist in the object\'s tag set in order for the
    -- rule to apply.
    tags :: Prelude.Maybe [Tag]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LifecycleRuleAndOperator' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'prefix', 'lifecycleRuleAndOperator_prefix' - Prefix identifying one or more objects to which the rule applies.
--
-- 'tags', 'lifecycleRuleAndOperator_tags' - All of these tags must exist in the object\'s tag set in order for the
-- rule to apply.
newLifecycleRuleAndOperator ::
  LifecycleRuleAndOperator
newLifecycleRuleAndOperator =
  LifecycleRuleAndOperator'
    { prefix = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | Prefix identifying one or more objects to which the rule applies.
lifecycleRuleAndOperator_prefix :: Lens.Lens' LifecycleRuleAndOperator (Prelude.Maybe Prelude.Text)
lifecycleRuleAndOperator_prefix = Lens.lens (\LifecycleRuleAndOperator' {prefix} -> prefix) (\s@LifecycleRuleAndOperator' {} a -> s {prefix = a} :: LifecycleRuleAndOperator)

-- | All of these tags must exist in the object\'s tag set in order for the
-- rule to apply.
lifecycleRuleAndOperator_tags :: Lens.Lens' LifecycleRuleAndOperator (Prelude.Maybe [Tag])
lifecycleRuleAndOperator_tags = Lens.lens (\LifecycleRuleAndOperator' {tags} -> tags) (\s@LifecycleRuleAndOperator' {} a -> s {tags = a} :: LifecycleRuleAndOperator) Prelude.. Lens.mapping Lens.coerced

instance Core.FromXML LifecycleRuleAndOperator where
  parseXML x =
    LifecycleRuleAndOperator'
      Prelude.<$> (x Core..@? "Prefix")
      Prelude.<*> ( x Core..@? "Tag" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "Tag")
                  )

instance Prelude.Hashable LifecycleRuleAndOperator where
  hashWithSalt _salt LifecycleRuleAndOperator' {..} =
    _salt `Prelude.hashWithSalt` prefix
      `Prelude.hashWithSalt` tags

instance Prelude.NFData LifecycleRuleAndOperator where
  rnf LifecycleRuleAndOperator' {..} =
    Prelude.rnf prefix `Prelude.seq` Prelude.rnf tags

instance Core.ToXML LifecycleRuleAndOperator where
  toXML LifecycleRuleAndOperator' {..} =
    Prelude.mconcat
      [ "Prefix" Core.@= prefix,
        "Tag"
          Core.@= Core.toXML (Core.toXMLList "Tag" Prelude.<$> tags)
      ]
