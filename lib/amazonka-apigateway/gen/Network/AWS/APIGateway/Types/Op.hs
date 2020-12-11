-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.Types.Op
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.APIGateway.Types.Op
  ( Op
      ( Op',
        Add,
        Copy,
        Move,
        Remove,
        Replace,
        Test
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype Op = Op' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern Add :: Op
pattern Add = Op' "add"

pattern Copy :: Op
pattern Copy = Op' "copy"

pattern Move :: Op
pattern Move = Op' "move"

pattern Remove :: Op
pattern Remove = Op' "remove"

pattern Replace :: Op
pattern Replace = Op' "replace"

pattern Test :: Op
pattern Test = Op' "test"

{-# COMPLETE
  Add,
  Copy,
  Move,
  Remove,
  Replace,
  Test,
  Op'
  #-}
