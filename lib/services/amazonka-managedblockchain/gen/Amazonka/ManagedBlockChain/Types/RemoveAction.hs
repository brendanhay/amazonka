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
-- Module      : Amazonka.ManagedBlockChain.Types.RemoveAction
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ManagedBlockChain.Types.RemoveAction where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An action to remove a member from a Managed Blockchain network as the
-- result of a removal proposal that is @APPROVED@. The member and all
-- associated resources are deleted from the network.
--
-- Applies only to Hyperledger Fabric.
--
-- /See:/ 'newRemoveAction' smart constructor.
data RemoveAction = RemoveAction'
  { -- | The unique identifier of the member to remove.
    memberId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RemoveAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'memberId', 'removeAction_memberId' - The unique identifier of the member to remove.
newRemoveAction ::
  -- | 'memberId'
  Prelude.Text ->
  RemoveAction
newRemoveAction pMemberId_ =
  RemoveAction' {memberId = pMemberId_}

-- | The unique identifier of the member to remove.
removeAction_memberId :: Lens.Lens' RemoveAction Prelude.Text
removeAction_memberId = Lens.lens (\RemoveAction' {memberId} -> memberId) (\s@RemoveAction' {} a -> s {memberId = a} :: RemoveAction)

instance Data.FromJSON RemoveAction where
  parseJSON =
    Data.withObject
      "RemoveAction"
      ( \x ->
          RemoveAction' Prelude.<$> (x Data..: "MemberId")
      )

instance Prelude.Hashable RemoveAction where
  hashWithSalt _salt RemoveAction' {..} =
    _salt `Prelude.hashWithSalt` memberId

instance Prelude.NFData RemoveAction where
  rnf RemoveAction' {..} = Prelude.rnf memberId

instance Data.ToJSON RemoveAction where
  toJSON RemoveAction' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("MemberId" Data..= memberId)]
      )
