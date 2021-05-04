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
-- Module      : Network.AWS.WAF.Types.ByteMatchSetUpdate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAF.Types.ByteMatchSetUpdate where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.WAF.Types.ByteMatchTuple
import Network.AWS.WAF.Types.ChangeAction

-- | This is __AWS WAF Classic__ documentation. For more information, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/classic-waf-chapter.html AWS WAF Classic>
-- in the developer guide.
--
-- __For the latest version of AWS WAF__, use the AWS WAFV2 API and see the
-- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-chapter.html AWS WAF Developer Guide>.
-- With the latest version, AWS WAF has a single set of endpoints for
-- regional and global use.
--
-- In an UpdateByteMatchSet request, @ByteMatchSetUpdate@ specifies whether
-- to insert or delete a ByteMatchTuple and includes the settings for the
-- @ByteMatchTuple@.
--
-- /See:/ 'newByteMatchSetUpdate' smart constructor.
data ByteMatchSetUpdate = ByteMatchSetUpdate'
  { -- | Specifies whether to insert or delete a ByteMatchTuple.
    action :: ChangeAction,
    -- | Information about the part of a web request that you want AWS WAF to
    -- inspect and the value that you want AWS WAF to search for. If you
    -- specify @DELETE@ for the value of @Action@, the @ByteMatchTuple@ values
    -- must exactly match the values in the @ByteMatchTuple@ that you want to
    -- delete from the @ByteMatchSet@.
    byteMatchTuple :: ByteMatchTuple
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ByteMatchSetUpdate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'action', 'byteMatchSetUpdate_action' - Specifies whether to insert or delete a ByteMatchTuple.
--
-- 'byteMatchTuple', 'byteMatchSetUpdate_byteMatchTuple' - Information about the part of a web request that you want AWS WAF to
-- inspect and the value that you want AWS WAF to search for. If you
-- specify @DELETE@ for the value of @Action@, the @ByteMatchTuple@ values
-- must exactly match the values in the @ByteMatchTuple@ that you want to
-- delete from the @ByteMatchSet@.
newByteMatchSetUpdate ::
  -- | 'action'
  ChangeAction ->
  -- | 'byteMatchTuple'
  ByteMatchTuple ->
  ByteMatchSetUpdate
newByteMatchSetUpdate pAction_ pByteMatchTuple_ =
  ByteMatchSetUpdate'
    { action = pAction_,
      byteMatchTuple = pByteMatchTuple_
    }

-- | Specifies whether to insert or delete a ByteMatchTuple.
byteMatchSetUpdate_action :: Lens.Lens' ByteMatchSetUpdate ChangeAction
byteMatchSetUpdate_action = Lens.lens (\ByteMatchSetUpdate' {action} -> action) (\s@ByteMatchSetUpdate' {} a -> s {action = a} :: ByteMatchSetUpdate)

-- | Information about the part of a web request that you want AWS WAF to
-- inspect and the value that you want AWS WAF to search for. If you
-- specify @DELETE@ for the value of @Action@, the @ByteMatchTuple@ values
-- must exactly match the values in the @ByteMatchTuple@ that you want to
-- delete from the @ByteMatchSet@.
byteMatchSetUpdate_byteMatchTuple :: Lens.Lens' ByteMatchSetUpdate ByteMatchTuple
byteMatchSetUpdate_byteMatchTuple = Lens.lens (\ByteMatchSetUpdate' {byteMatchTuple} -> byteMatchTuple) (\s@ByteMatchSetUpdate' {} a -> s {byteMatchTuple = a} :: ByteMatchSetUpdate)

instance Prelude.Hashable ByteMatchSetUpdate

instance Prelude.NFData ByteMatchSetUpdate

instance Prelude.ToJSON ByteMatchSetUpdate where
  toJSON ByteMatchSetUpdate' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Action" Prelude..= action),
            Prelude.Just
              ("ByteMatchTuple" Prelude..= byteMatchTuple)
          ]
      )
