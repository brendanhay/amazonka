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
-- Module      : Amazonka.WAFV2.Types.WebACLSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WAFV2.Types.WebACLSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | High-level information about a WebACL, returned by operations like
-- create and list. This provides information like the ID, that you can use
-- to retrieve and manage a @WebACL@, and the ARN, that you provide to
-- operations like AssociateWebACL.
--
-- /See:/ 'newWebACLSummary' smart constructor.
data WebACLSummary = WebACLSummary'
  { -- | The Amazon Resource Name (ARN) of the entity.
    arn :: Prelude.Maybe Prelude.Text,
    -- | A description of the web ACL that helps with identification.
    description :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the web ACL. This ID is returned in the
    -- responses to create and list commands. You provide it to operations like
    -- update and delete.
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
    -- | The name of the web ACL. You cannot change the name of a web ACL after
    -- you create it.
    name :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'WebACLSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'webACLSummary_arn' - The Amazon Resource Name (ARN) of the entity.
--
-- 'description', 'webACLSummary_description' - A description of the web ACL that helps with identification.
--
-- 'id', 'webACLSummary_id' - The unique identifier for the web ACL. This ID is returned in the
-- responses to create and list commands. You provide it to operations like
-- update and delete.
--
-- 'lockToken', 'webACLSummary_lockToken' - A token used for optimistic locking. WAF returns a token to your @get@
-- and @list@ requests, to mark the state of the entity at the time of the
-- request. To make changes to the entity associated with the token, you
-- provide the token to operations like @update@ and @delete@. WAF uses the
-- token to ensure that no changes have been made to the entity since you
-- last retrieved it. If a change has been made, the update fails with a
-- @WAFOptimisticLockException@. If this happens, perform another @get@,
-- and use the new token returned by that operation.
--
-- 'name', 'webACLSummary_name' - The name of the web ACL. You cannot change the name of a web ACL after
-- you create it.
newWebACLSummary ::
  WebACLSummary
newWebACLSummary =
  WebACLSummary'
    { arn = Prelude.Nothing,
      description = Prelude.Nothing,
      id = Prelude.Nothing,
      lockToken = Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the entity.
webACLSummary_arn :: Lens.Lens' WebACLSummary (Prelude.Maybe Prelude.Text)
webACLSummary_arn = Lens.lens (\WebACLSummary' {arn} -> arn) (\s@WebACLSummary' {} a -> s {arn = a} :: WebACLSummary)

-- | A description of the web ACL that helps with identification.
webACLSummary_description :: Lens.Lens' WebACLSummary (Prelude.Maybe Prelude.Text)
webACLSummary_description = Lens.lens (\WebACLSummary' {description} -> description) (\s@WebACLSummary' {} a -> s {description = a} :: WebACLSummary)

-- | The unique identifier for the web ACL. This ID is returned in the
-- responses to create and list commands. You provide it to operations like
-- update and delete.
webACLSummary_id :: Lens.Lens' WebACLSummary (Prelude.Maybe Prelude.Text)
webACLSummary_id = Lens.lens (\WebACLSummary' {id} -> id) (\s@WebACLSummary' {} a -> s {id = a} :: WebACLSummary)

-- | A token used for optimistic locking. WAF returns a token to your @get@
-- and @list@ requests, to mark the state of the entity at the time of the
-- request. To make changes to the entity associated with the token, you
-- provide the token to operations like @update@ and @delete@. WAF uses the
-- token to ensure that no changes have been made to the entity since you
-- last retrieved it. If a change has been made, the update fails with a
-- @WAFOptimisticLockException@. If this happens, perform another @get@,
-- and use the new token returned by that operation.
webACLSummary_lockToken :: Lens.Lens' WebACLSummary (Prelude.Maybe Prelude.Text)
webACLSummary_lockToken = Lens.lens (\WebACLSummary' {lockToken} -> lockToken) (\s@WebACLSummary' {} a -> s {lockToken = a} :: WebACLSummary)

-- | The name of the web ACL. You cannot change the name of a web ACL after
-- you create it.
webACLSummary_name :: Lens.Lens' WebACLSummary (Prelude.Maybe Prelude.Text)
webACLSummary_name = Lens.lens (\WebACLSummary' {name} -> name) (\s@WebACLSummary' {} a -> s {name = a} :: WebACLSummary)

instance Data.FromJSON WebACLSummary where
  parseJSON =
    Data.withObject
      "WebACLSummary"
      ( \x ->
          WebACLSummary'
            Prelude.<$> (x Data..:? "ARN")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "Id")
            Prelude.<*> (x Data..:? "LockToken")
            Prelude.<*> (x Data..:? "Name")
      )

instance Prelude.Hashable WebACLSummary where
  hashWithSalt _salt WebACLSummary' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` lockToken
      `Prelude.hashWithSalt` name

instance Prelude.NFData WebACLSummary where
  rnf WebACLSummary' {..} =
    Prelude.rnf arn `Prelude.seq`
      Prelude.rnf description `Prelude.seq`
        Prelude.rnf id `Prelude.seq`
          Prelude.rnf lockToken `Prelude.seq`
            Prelude.rnf name
