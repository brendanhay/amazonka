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
-- Module      : Amazonka.WAFV2.Types.RegexPatternSet
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WAFV2.Types.RegexPatternSet where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.WAFV2.Types.Regex

-- | Contains one or more regular expressions.
--
-- WAF assigns an ARN to each @RegexPatternSet@ that you create. To use a
-- set in a rule, you provide the ARN to the Rule statement
-- RegexPatternSetReferenceStatement.
--
-- /See:/ 'newRegexPatternSet' smart constructor.
data RegexPatternSet = RegexPatternSet'
  { -- | The Amazon Resource Name (ARN) of the entity.
    arn :: Prelude.Maybe Prelude.Text,
    -- | A description of the set that helps with identification.
    description :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the set. This ID is returned in the responses to
    -- create and list commands. You provide it to operations like update and
    -- delete.
    id :: Prelude.Maybe Prelude.Text,
    -- | The name of the set. You cannot change the name after you create the
    -- set.
    name :: Prelude.Maybe Prelude.Text,
    -- | The regular expression patterns in the set.
    regularExpressionList :: Prelude.Maybe [Regex]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RegexPatternSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'regexPatternSet_arn' - The Amazon Resource Name (ARN) of the entity.
--
-- 'description', 'regexPatternSet_description' - A description of the set that helps with identification.
--
-- 'id', 'regexPatternSet_id' - A unique identifier for the set. This ID is returned in the responses to
-- create and list commands. You provide it to operations like update and
-- delete.
--
-- 'name', 'regexPatternSet_name' - The name of the set. You cannot change the name after you create the
-- set.
--
-- 'regularExpressionList', 'regexPatternSet_regularExpressionList' - The regular expression patterns in the set.
newRegexPatternSet ::
  RegexPatternSet
newRegexPatternSet =
  RegexPatternSet'
    { arn = Prelude.Nothing,
      description = Prelude.Nothing,
      id = Prelude.Nothing,
      name = Prelude.Nothing,
      regularExpressionList = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the entity.
regexPatternSet_arn :: Lens.Lens' RegexPatternSet (Prelude.Maybe Prelude.Text)
regexPatternSet_arn = Lens.lens (\RegexPatternSet' {arn} -> arn) (\s@RegexPatternSet' {} a -> s {arn = a} :: RegexPatternSet)

-- | A description of the set that helps with identification.
regexPatternSet_description :: Lens.Lens' RegexPatternSet (Prelude.Maybe Prelude.Text)
regexPatternSet_description = Lens.lens (\RegexPatternSet' {description} -> description) (\s@RegexPatternSet' {} a -> s {description = a} :: RegexPatternSet)

-- | A unique identifier for the set. This ID is returned in the responses to
-- create and list commands. You provide it to operations like update and
-- delete.
regexPatternSet_id :: Lens.Lens' RegexPatternSet (Prelude.Maybe Prelude.Text)
regexPatternSet_id = Lens.lens (\RegexPatternSet' {id} -> id) (\s@RegexPatternSet' {} a -> s {id = a} :: RegexPatternSet)

-- | The name of the set. You cannot change the name after you create the
-- set.
regexPatternSet_name :: Lens.Lens' RegexPatternSet (Prelude.Maybe Prelude.Text)
regexPatternSet_name = Lens.lens (\RegexPatternSet' {name} -> name) (\s@RegexPatternSet' {} a -> s {name = a} :: RegexPatternSet)

-- | The regular expression patterns in the set.
regexPatternSet_regularExpressionList :: Lens.Lens' RegexPatternSet (Prelude.Maybe [Regex])
regexPatternSet_regularExpressionList = Lens.lens (\RegexPatternSet' {regularExpressionList} -> regularExpressionList) (\s@RegexPatternSet' {} a -> s {regularExpressionList = a} :: RegexPatternSet) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON RegexPatternSet where
  parseJSON =
    Data.withObject
      "RegexPatternSet"
      ( \x ->
          RegexPatternSet'
            Prelude.<$> (x Data..:? "ARN")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "Id")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> ( x
                            Data..:? "RegularExpressionList"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable RegexPatternSet where
  hashWithSalt _salt RegexPatternSet' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` regularExpressionList

instance Prelude.NFData RegexPatternSet where
  rnf RegexPatternSet' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf regularExpressionList
