{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Organizations.Types.HandshakeParty
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Organizations.Types.HandshakeParty where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Organizations.Types.HandshakePartyType
import qualified Network.AWS.Prelude as Prelude

-- | Identifies a participant in a handshake.
--
-- /See:/ 'newHandshakeParty' smart constructor.
data HandshakeParty = HandshakeParty'
  { -- | The unique identifier (ID) for the party.
    --
    -- The <http://wikipedia.org/wiki/regex regex pattern> for handshake ID
    -- string requires \"h-\" followed by from 8 to 32 lowercase letters or
    -- digits.
    id :: Prelude.Sensitive Prelude.Text,
    -- | The type of party.
    type' :: HandshakePartyType
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'HandshakeParty' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'handshakeParty_id' - The unique identifier (ID) for the party.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for handshake ID
-- string requires \"h-\" followed by from 8 to 32 lowercase letters or
-- digits.
--
-- 'type'', 'handshakeParty_type' - The type of party.
newHandshakeParty ::
  -- | 'id'
  Prelude.Text ->
  -- | 'type''
  HandshakePartyType ->
  HandshakeParty
newHandshakeParty pId_ pType_ =
  HandshakeParty'
    { id =
        Prelude._Sensitive Lens.# pId_,
      type' = pType_
    }

-- | The unique identifier (ID) for the party.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for handshake ID
-- string requires \"h-\" followed by from 8 to 32 lowercase letters or
-- digits.
handshakeParty_id :: Lens.Lens' HandshakeParty Prelude.Text
handshakeParty_id = Lens.lens (\HandshakeParty' {id} -> id) (\s@HandshakeParty' {} a -> s {id = a} :: HandshakeParty) Prelude.. Prelude._Sensitive

-- | The type of party.
handshakeParty_type :: Lens.Lens' HandshakeParty HandshakePartyType
handshakeParty_type = Lens.lens (\HandshakeParty' {type'} -> type') (\s@HandshakeParty' {} a -> s {type' = a} :: HandshakeParty)

instance Prelude.FromJSON HandshakeParty where
  parseJSON =
    Prelude.withObject
      "HandshakeParty"
      ( \x ->
          HandshakeParty'
            Prelude.<$> (x Prelude..: "Id")
            Prelude.<*> (x Prelude..: "Type")
      )

instance Prelude.Hashable HandshakeParty

instance Prelude.NFData HandshakeParty

instance Prelude.ToJSON HandshakeParty where
  toJSON HandshakeParty' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Id" Prelude..= id),
            Prelude.Just ("Type" Prelude..= type')
          ]
      )
