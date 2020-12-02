{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.Types.ByteMatchSetUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAF.Types.ByteMatchSetUpdate where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.WAF.Types.ByteMatchTuple
import Network.AWS.WAF.Types.ChangeAction

-- | In an 'UpdateByteMatchSet' request, @ByteMatchSetUpdate@ specifies whether to insert or delete a 'ByteMatchTuple' and includes the settings for the @ByteMatchTuple@ .
--
--
--
-- /See:/ 'byteMatchSetUpdate' smart constructor.
data ByteMatchSetUpdate = ByteMatchSetUpdate'
  { _bmsuAction ::
      !ChangeAction,
    _bmsuByteMatchTuple :: !ByteMatchTuple
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ByteMatchSetUpdate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bmsuAction' - Specifies whether to insert or delete a 'ByteMatchTuple' .
--
-- * 'bmsuByteMatchTuple' - Information about the part of a web request that you want AWS WAF to inspect and the value that you want AWS WAF to search for. If you specify @DELETE@ for the value of @Action@ , the @ByteMatchTuple@ values must exactly match the values in the @ByteMatchTuple@ that you want to delete from the @ByteMatchSet@ .
byteMatchSetUpdate ::
  -- | 'bmsuAction'
  ChangeAction ->
  -- | 'bmsuByteMatchTuple'
  ByteMatchTuple ->
  ByteMatchSetUpdate
byteMatchSetUpdate pAction_ pByteMatchTuple_ =
  ByteMatchSetUpdate'
    { _bmsuAction = pAction_,
      _bmsuByteMatchTuple = pByteMatchTuple_
    }

-- | Specifies whether to insert or delete a 'ByteMatchTuple' .
bmsuAction :: Lens' ByteMatchSetUpdate ChangeAction
bmsuAction = lens _bmsuAction (\s a -> s {_bmsuAction = a})

-- | Information about the part of a web request that you want AWS WAF to inspect and the value that you want AWS WAF to search for. If you specify @DELETE@ for the value of @Action@ , the @ByteMatchTuple@ values must exactly match the values in the @ByteMatchTuple@ that you want to delete from the @ByteMatchSet@ .
bmsuByteMatchTuple :: Lens' ByteMatchSetUpdate ByteMatchTuple
bmsuByteMatchTuple = lens _bmsuByteMatchTuple (\s a -> s {_bmsuByteMatchTuple = a})

instance Hashable ByteMatchSetUpdate

instance NFData ByteMatchSetUpdate

instance ToJSON ByteMatchSetUpdate where
  toJSON ByteMatchSetUpdate' {..} =
    object
      ( catMaybes
          [ Just ("Action" .= _bmsuAction),
            Just ("ByteMatchTuple" .= _bmsuByteMatchTuple)
          ]
      )
