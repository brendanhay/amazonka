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
-- Module      : Amazonka.ManagedBlockChain.Types.InviteAction
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ManagedBlockChain.Types.InviteAction where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An action to invite a specific Amazon Web Services account to create a
-- member and join the network. The @InviteAction@ is carried out when a
-- @Proposal@ is @APPROVED@.
--
-- Applies only to Hyperledger Fabric.
--
-- /See:/ 'newInviteAction' smart constructor.
data InviteAction = InviteAction'
  { -- | The Amazon Web Services account ID to invite.
    principal :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InviteAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'principal', 'inviteAction_principal' - The Amazon Web Services account ID to invite.
newInviteAction ::
  -- | 'principal'
  Prelude.Text ->
  InviteAction
newInviteAction pPrincipal_ =
  InviteAction' {principal = pPrincipal_}

-- | The Amazon Web Services account ID to invite.
inviteAction_principal :: Lens.Lens' InviteAction Prelude.Text
inviteAction_principal = Lens.lens (\InviteAction' {principal} -> principal) (\s@InviteAction' {} a -> s {principal = a} :: InviteAction)

instance Data.FromJSON InviteAction where
  parseJSON =
    Data.withObject
      "InviteAction"
      ( \x ->
          InviteAction' Prelude.<$> (x Data..: "Principal")
      )

instance Prelude.Hashable InviteAction where
  hashWithSalt _salt InviteAction' {..} =
    _salt `Prelude.hashWithSalt` principal

instance Prelude.NFData InviteAction where
  rnf InviteAction' {..} = Prelude.rnf principal

instance Data.ToJSON InviteAction where
  toJSON InviteAction' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Principal" Data..= principal)]
      )
