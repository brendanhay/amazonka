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
-- Module      : Network.AWS.Organizations.Types.HandshakeFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Organizations.Types.HandshakeFilter where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Organizations.Types.ActionType
import qualified Network.AWS.Prelude as Prelude

-- | Specifies the criteria that are used to select the handshakes for the
-- operation.
--
-- /See:/ 'newHandshakeFilter' smart constructor.
data HandshakeFilter = HandshakeFilter'
  { -- | Specifies the type of handshake action.
    --
    -- If you specify @ActionType@, you cannot also specify
    -- @ParentHandshakeId@.
    actionType :: Prelude.Maybe ActionType,
    -- | Specifies the parent handshake. Only used for handshake types that are a
    -- child of another type.
    --
    -- If you specify @ParentHandshakeId@, you cannot also specify
    -- @ActionType@.
    --
    -- The <http://wikipedia.org/wiki/regex regex pattern> for handshake ID
    -- string requires \"h-\" followed by from 8 to 32 lowercase letters or
    -- digits.
    parentHandshakeId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'HandshakeFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'actionType', 'handshakeFilter_actionType' - Specifies the type of handshake action.
--
-- If you specify @ActionType@, you cannot also specify
-- @ParentHandshakeId@.
--
-- 'parentHandshakeId', 'handshakeFilter_parentHandshakeId' - Specifies the parent handshake. Only used for handshake types that are a
-- child of another type.
--
-- If you specify @ParentHandshakeId@, you cannot also specify
-- @ActionType@.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for handshake ID
-- string requires \"h-\" followed by from 8 to 32 lowercase letters or
-- digits.
newHandshakeFilter ::
  HandshakeFilter
newHandshakeFilter =
  HandshakeFilter'
    { actionType = Prelude.Nothing,
      parentHandshakeId = Prelude.Nothing
    }

-- | Specifies the type of handshake action.
--
-- If you specify @ActionType@, you cannot also specify
-- @ParentHandshakeId@.
handshakeFilter_actionType :: Lens.Lens' HandshakeFilter (Prelude.Maybe ActionType)
handshakeFilter_actionType = Lens.lens (\HandshakeFilter' {actionType} -> actionType) (\s@HandshakeFilter' {} a -> s {actionType = a} :: HandshakeFilter)

-- | Specifies the parent handshake. Only used for handshake types that are a
-- child of another type.
--
-- If you specify @ParentHandshakeId@, you cannot also specify
-- @ActionType@.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for handshake ID
-- string requires \"h-\" followed by from 8 to 32 lowercase letters or
-- digits.
handshakeFilter_parentHandshakeId :: Lens.Lens' HandshakeFilter (Prelude.Maybe Prelude.Text)
handshakeFilter_parentHandshakeId = Lens.lens (\HandshakeFilter' {parentHandshakeId} -> parentHandshakeId) (\s@HandshakeFilter' {} a -> s {parentHandshakeId = a} :: HandshakeFilter)

instance Prelude.Hashable HandshakeFilter

instance Prelude.NFData HandshakeFilter

instance Prelude.ToJSON HandshakeFilter where
  toJSON HandshakeFilter' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("ActionType" Prelude..=) Prelude.<$> actionType,
            ("ParentHandshakeId" Prelude..=)
              Prelude.<$> parentHandshakeId
          ]
      )
