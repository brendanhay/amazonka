-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.UpdateBehavior
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.UpdateBehavior
  ( UpdateBehavior
      ( UpdateBehavior',
        UBLog,
        UBUpdateInDatabase
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype UpdateBehavior = UpdateBehavior' Lude.Text
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

pattern UBLog :: UpdateBehavior
pattern UBLog = UpdateBehavior' "LOG"

pattern UBUpdateInDatabase :: UpdateBehavior
pattern UBUpdateInDatabase = UpdateBehavior' "UPDATE_IN_DATABASE"

{-# COMPLETE
  UBLog,
  UBUpdateInDatabase,
  UpdateBehavior'
  #-}
