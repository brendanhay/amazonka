-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.ChangeAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.ChangeAction
  ( ChangeAction
      ( ChangeAction',
        Add,
        Modify,
        Remove
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ChangeAction = ChangeAction' Lude.Text
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

pattern Add :: ChangeAction
pattern Add = ChangeAction' "ADD"

pattern Modify :: ChangeAction
pattern Modify = ChangeAction' "MODIFY"

pattern Remove :: ChangeAction
pattern Remove = ChangeAction' "REMOVE"

{-# COMPLETE
  Add,
  Modify,
  Remove,
  ChangeAction'
  #-}
