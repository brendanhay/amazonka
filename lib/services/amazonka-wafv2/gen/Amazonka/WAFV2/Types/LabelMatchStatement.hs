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
-- Module      : Amazonka.WAFV2.Types.LabelMatchStatement
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WAFV2.Types.LabelMatchStatement where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.WAFV2.Types.LabelMatchScope

-- | A rule statement to match against labels that have been added to the web
-- request by rules that have already run in the web ACL.
--
-- The label match statement provides the label or namespace string to
-- search for. The label string can represent a part or all of the fully
-- qualified label name that had been added to the web request. Fully
-- qualified labels have a prefix, optional namespaces, and label name. The
-- prefix identifies the rule group or web ACL context of the rule that
-- added the label. If you do not provide the fully qualified name in your
-- label match string, WAF performs the search for labels that were added
-- in the same context as the label match statement.
--
-- /See:/ 'newLabelMatchStatement' smart constructor.
data LabelMatchStatement = LabelMatchStatement'
  { -- | Specify whether you want to match using the label name or just the
    -- namespace.
    scope :: LabelMatchScope,
    -- | The string to match against. The setting you provide for this depends on
    -- the match statement\'s @Scope@ setting:
    --
    -- -   If the @Scope@ indicates @LABEL@, then this specification must
    --     include the name and can include any number of preceding namespace
    --     specifications and prefix up to providing the fully qualified label
    --     name.
    --
    -- -   If the @Scope@ indicates @NAMESPACE@, then this specification can
    --     include any number of contiguous namespace strings, and can include
    --     the entire label namespace prefix from the rule group or web ACL
    --     where the label originates.
    --
    -- Labels are case sensitive and components of a label must be separated by
    -- colon, for example @NS1:NS2:name@.
    key :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LabelMatchStatement' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'scope', 'labelMatchStatement_scope' - Specify whether you want to match using the label name or just the
-- namespace.
--
-- 'key', 'labelMatchStatement_key' - The string to match against. The setting you provide for this depends on
-- the match statement\'s @Scope@ setting:
--
-- -   If the @Scope@ indicates @LABEL@, then this specification must
--     include the name and can include any number of preceding namespace
--     specifications and prefix up to providing the fully qualified label
--     name.
--
-- -   If the @Scope@ indicates @NAMESPACE@, then this specification can
--     include any number of contiguous namespace strings, and can include
--     the entire label namespace prefix from the rule group or web ACL
--     where the label originates.
--
-- Labels are case sensitive and components of a label must be separated by
-- colon, for example @NS1:NS2:name@.
newLabelMatchStatement ::
  -- | 'scope'
  LabelMatchScope ->
  -- | 'key'
  Prelude.Text ->
  LabelMatchStatement
newLabelMatchStatement pScope_ pKey_ =
  LabelMatchStatement' {scope = pScope_, key = pKey_}

-- | Specify whether you want to match using the label name or just the
-- namespace.
labelMatchStatement_scope :: Lens.Lens' LabelMatchStatement LabelMatchScope
labelMatchStatement_scope = Lens.lens (\LabelMatchStatement' {scope} -> scope) (\s@LabelMatchStatement' {} a -> s {scope = a} :: LabelMatchStatement)

-- | The string to match against. The setting you provide for this depends on
-- the match statement\'s @Scope@ setting:
--
-- -   If the @Scope@ indicates @LABEL@, then this specification must
--     include the name and can include any number of preceding namespace
--     specifications and prefix up to providing the fully qualified label
--     name.
--
-- -   If the @Scope@ indicates @NAMESPACE@, then this specification can
--     include any number of contiguous namespace strings, and can include
--     the entire label namespace prefix from the rule group or web ACL
--     where the label originates.
--
-- Labels are case sensitive and components of a label must be separated by
-- colon, for example @NS1:NS2:name@.
labelMatchStatement_key :: Lens.Lens' LabelMatchStatement Prelude.Text
labelMatchStatement_key = Lens.lens (\LabelMatchStatement' {key} -> key) (\s@LabelMatchStatement' {} a -> s {key = a} :: LabelMatchStatement)

instance Data.FromJSON LabelMatchStatement where
  parseJSON =
    Data.withObject
      "LabelMatchStatement"
      ( \x ->
          LabelMatchStatement'
            Prelude.<$> (x Data..: "Scope") Prelude.<*> (x Data..: "Key")
      )

instance Prelude.Hashable LabelMatchStatement where
  hashWithSalt _salt LabelMatchStatement' {..} =
    _salt `Prelude.hashWithSalt` scope
      `Prelude.hashWithSalt` key

instance Prelude.NFData LabelMatchStatement where
  rnf LabelMatchStatement' {..} =
    Prelude.rnf scope `Prelude.seq` Prelude.rnf key

instance Data.ToJSON LabelMatchStatement where
  toJSON LabelMatchStatement' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Scope" Data..= scope),
            Prelude.Just ("Key" Data..= key)
          ]
      )
