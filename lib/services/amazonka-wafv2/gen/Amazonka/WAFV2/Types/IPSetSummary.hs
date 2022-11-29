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
-- Module      : Amazonka.WAFV2.Types.IPSetSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WAFV2.Types.IPSetSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | High-level information about an IPSet, returned by operations like
-- create and list. This provides information like the ID, that you can use
-- to retrieve and manage an @IPSet@, and the ARN, that you provide to the
-- IPSetReferenceStatement to use the address set in a Rule.
--
-- /See:/ 'newIPSetSummary' smart constructor.
data IPSetSummary = IPSetSummary'
  { -- | The name of the IP set. You cannot change the name of an @IPSet@ after
    -- you create it.
    name :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the entity.
    arn :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the set. This ID is returned in the responses to
    -- create and list commands. You provide it to operations like update and
    -- delete.
    id :: Prelude.Maybe Prelude.Text,
    -- | A description of the IP set that helps with identification.
    description :: Prelude.Maybe Prelude.Text,
    -- | A token used for optimistic locking. WAF returns a token to your @get@
    -- and @list@ requests, to mark the state of the entity at the time of the
    -- request. To make changes to the entity associated with the token, you
    -- provide the token to operations like @update@ and @delete@. WAF uses the
    -- token to ensure that no changes have been made to the entity since you
    -- last retrieved it. If a change has been made, the update fails with a
    -- @WAFOptimisticLockException@. If this happens, perform another @get@,
    -- and use the new token returned by that operation.
    lockToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IPSetSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'iPSetSummary_name' - The name of the IP set. You cannot change the name of an @IPSet@ after
-- you create it.
--
-- 'arn', 'iPSetSummary_arn' - The Amazon Resource Name (ARN) of the entity.
--
-- 'id', 'iPSetSummary_id' - A unique identifier for the set. This ID is returned in the responses to
-- create and list commands. You provide it to operations like update and
-- delete.
--
-- 'description', 'iPSetSummary_description' - A description of the IP set that helps with identification.
--
-- 'lockToken', 'iPSetSummary_lockToken' - A token used for optimistic locking. WAF returns a token to your @get@
-- and @list@ requests, to mark the state of the entity at the time of the
-- request. To make changes to the entity associated with the token, you
-- provide the token to operations like @update@ and @delete@. WAF uses the
-- token to ensure that no changes have been made to the entity since you
-- last retrieved it. If a change has been made, the update fails with a
-- @WAFOptimisticLockException@. If this happens, perform another @get@,
-- and use the new token returned by that operation.
newIPSetSummary ::
  IPSetSummary
newIPSetSummary =
  IPSetSummary'
    { name = Prelude.Nothing,
      arn = Prelude.Nothing,
      id = Prelude.Nothing,
      description = Prelude.Nothing,
      lockToken = Prelude.Nothing
    }

-- | The name of the IP set. You cannot change the name of an @IPSet@ after
-- you create it.
iPSetSummary_name :: Lens.Lens' IPSetSummary (Prelude.Maybe Prelude.Text)
iPSetSummary_name = Lens.lens (\IPSetSummary' {name} -> name) (\s@IPSetSummary' {} a -> s {name = a} :: IPSetSummary)

-- | The Amazon Resource Name (ARN) of the entity.
iPSetSummary_arn :: Lens.Lens' IPSetSummary (Prelude.Maybe Prelude.Text)
iPSetSummary_arn = Lens.lens (\IPSetSummary' {arn} -> arn) (\s@IPSetSummary' {} a -> s {arn = a} :: IPSetSummary)

-- | A unique identifier for the set. This ID is returned in the responses to
-- create and list commands. You provide it to operations like update and
-- delete.
iPSetSummary_id :: Lens.Lens' IPSetSummary (Prelude.Maybe Prelude.Text)
iPSetSummary_id = Lens.lens (\IPSetSummary' {id} -> id) (\s@IPSetSummary' {} a -> s {id = a} :: IPSetSummary)

-- | A description of the IP set that helps with identification.
iPSetSummary_description :: Lens.Lens' IPSetSummary (Prelude.Maybe Prelude.Text)
iPSetSummary_description = Lens.lens (\IPSetSummary' {description} -> description) (\s@IPSetSummary' {} a -> s {description = a} :: IPSetSummary)

-- | A token used for optimistic locking. WAF returns a token to your @get@
-- and @list@ requests, to mark the state of the entity at the time of the
-- request. To make changes to the entity associated with the token, you
-- provide the token to operations like @update@ and @delete@. WAF uses the
-- token to ensure that no changes have been made to the entity since you
-- last retrieved it. If a change has been made, the update fails with a
-- @WAFOptimisticLockException@. If this happens, perform another @get@,
-- and use the new token returned by that operation.
iPSetSummary_lockToken :: Lens.Lens' IPSetSummary (Prelude.Maybe Prelude.Text)
iPSetSummary_lockToken = Lens.lens (\IPSetSummary' {lockToken} -> lockToken) (\s@IPSetSummary' {} a -> s {lockToken = a} :: IPSetSummary)

instance Core.FromJSON IPSetSummary where
  parseJSON =
    Core.withObject
      "IPSetSummary"
      ( \x ->
          IPSetSummary'
            Prelude.<$> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "ARN")
            Prelude.<*> (x Core..:? "Id")
            Prelude.<*> (x Core..:? "Description")
            Prelude.<*> (x Core..:? "LockToken")
      )

instance Prelude.Hashable IPSetSummary where
  hashWithSalt _salt IPSetSummary' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` lockToken

instance Prelude.NFData IPSetSummary where
  rnf IPSetSummary' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf lockToken
