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
-- Module      : Amazonka.WAFV2.Types.RegexPatternSetSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WAFV2.Types.RegexPatternSetSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | High-level information about a RegexPatternSet, returned by operations
-- like create and list. This provides information like the ID, that you
-- can use to retrieve and manage a @RegexPatternSet@, and the ARN, that
-- you provide to the RegexPatternSetReferenceStatement to use the pattern
-- set in a Rule.
--
-- /See:/ 'newRegexPatternSetSummary' smart constructor.
data RegexPatternSetSummary = RegexPatternSetSummary'
  { -- | The Amazon Resource Name (ARN) of the entity.
    arn :: Prelude.Maybe Prelude.Text,
    -- | A description of the set that helps with identification.
    description :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the set. This ID is returned in the responses to
    -- create and list commands. You provide it to operations like update and
    -- delete.
    id :: Prelude.Maybe Prelude.Text,
    -- | A token used for optimistic locking. WAF returns a token to your @get@
    -- and @list@ requests, to mark the state of the entity at the time of the
    -- request. To make changes to the entity associated with the token, you
    -- provide the token to operations like @update@ and @delete@. WAF uses the
    -- token to ensure that no changes have been made to the entity since you
    -- last retrieved it. If a change has been made, the update fails with a
    -- @WAFOptimisticLockException@. If this happens, perform another @get@,
    -- and use the new token returned by that operation.
    lockToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the data type instance. You cannot change the name after you
    -- create the instance.
    name :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RegexPatternSetSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'regexPatternSetSummary_arn' - The Amazon Resource Name (ARN) of the entity.
--
-- 'description', 'regexPatternSetSummary_description' - A description of the set that helps with identification.
--
-- 'id', 'regexPatternSetSummary_id' - A unique identifier for the set. This ID is returned in the responses to
-- create and list commands. You provide it to operations like update and
-- delete.
--
-- 'lockToken', 'regexPatternSetSummary_lockToken' - A token used for optimistic locking. WAF returns a token to your @get@
-- and @list@ requests, to mark the state of the entity at the time of the
-- request. To make changes to the entity associated with the token, you
-- provide the token to operations like @update@ and @delete@. WAF uses the
-- token to ensure that no changes have been made to the entity since you
-- last retrieved it. If a change has been made, the update fails with a
-- @WAFOptimisticLockException@. If this happens, perform another @get@,
-- and use the new token returned by that operation.
--
-- 'name', 'regexPatternSetSummary_name' - The name of the data type instance. You cannot change the name after you
-- create the instance.
newRegexPatternSetSummary ::
  RegexPatternSetSummary
newRegexPatternSetSummary =
  RegexPatternSetSummary'
    { arn = Prelude.Nothing,
      description = Prelude.Nothing,
      id = Prelude.Nothing,
      lockToken = Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the entity.
regexPatternSetSummary_arn :: Lens.Lens' RegexPatternSetSummary (Prelude.Maybe Prelude.Text)
regexPatternSetSummary_arn = Lens.lens (\RegexPatternSetSummary' {arn} -> arn) (\s@RegexPatternSetSummary' {} a -> s {arn = a} :: RegexPatternSetSummary)

-- | A description of the set that helps with identification.
regexPatternSetSummary_description :: Lens.Lens' RegexPatternSetSummary (Prelude.Maybe Prelude.Text)
regexPatternSetSummary_description = Lens.lens (\RegexPatternSetSummary' {description} -> description) (\s@RegexPatternSetSummary' {} a -> s {description = a} :: RegexPatternSetSummary)

-- | A unique identifier for the set. This ID is returned in the responses to
-- create and list commands. You provide it to operations like update and
-- delete.
regexPatternSetSummary_id :: Lens.Lens' RegexPatternSetSummary (Prelude.Maybe Prelude.Text)
regexPatternSetSummary_id = Lens.lens (\RegexPatternSetSummary' {id} -> id) (\s@RegexPatternSetSummary' {} a -> s {id = a} :: RegexPatternSetSummary)

-- | A token used for optimistic locking. WAF returns a token to your @get@
-- and @list@ requests, to mark the state of the entity at the time of the
-- request. To make changes to the entity associated with the token, you
-- provide the token to operations like @update@ and @delete@. WAF uses the
-- token to ensure that no changes have been made to the entity since you
-- last retrieved it. If a change has been made, the update fails with a
-- @WAFOptimisticLockException@. If this happens, perform another @get@,
-- and use the new token returned by that operation.
regexPatternSetSummary_lockToken :: Lens.Lens' RegexPatternSetSummary (Prelude.Maybe Prelude.Text)
regexPatternSetSummary_lockToken = Lens.lens (\RegexPatternSetSummary' {lockToken} -> lockToken) (\s@RegexPatternSetSummary' {} a -> s {lockToken = a} :: RegexPatternSetSummary)

-- | The name of the data type instance. You cannot change the name after you
-- create the instance.
regexPatternSetSummary_name :: Lens.Lens' RegexPatternSetSummary (Prelude.Maybe Prelude.Text)
regexPatternSetSummary_name = Lens.lens (\RegexPatternSetSummary' {name} -> name) (\s@RegexPatternSetSummary' {} a -> s {name = a} :: RegexPatternSetSummary)

instance Data.FromJSON RegexPatternSetSummary where
  parseJSON =
    Data.withObject
      "RegexPatternSetSummary"
      ( \x ->
          RegexPatternSetSummary'
            Prelude.<$> (x Data..:? "ARN")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "Id")
            Prelude.<*> (x Data..:? "LockToken")
            Prelude.<*> (x Data..:? "Name")
      )

instance Prelude.Hashable RegexPatternSetSummary where
  hashWithSalt _salt RegexPatternSetSummary' {..} =
    _salt `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` lockToken
      `Prelude.hashWithSalt` name

instance Prelude.NFData RegexPatternSetSummary where
  rnf RegexPatternSetSummary' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf lockToken
      `Prelude.seq` Prelude.rnf name
