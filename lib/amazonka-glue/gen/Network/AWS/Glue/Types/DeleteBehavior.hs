{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.DeleteBehavior
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.DeleteBehavior
  ( DeleteBehavior
      ( DeleteBehavior',
        Log,
        DeleteFromDatabase,
        DeprecateInDatabase
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype DeleteBehavior = DeleteBehavior' Lude.Text
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

pattern Log :: DeleteBehavior
pattern Log = DeleteBehavior' "LOG"

pattern DeleteFromDatabase :: DeleteBehavior
pattern DeleteFromDatabase = DeleteBehavior' "DELETE_FROM_DATABASE"

pattern DeprecateInDatabase :: DeleteBehavior
pattern DeprecateInDatabase = DeleteBehavior' "DEPRECATE_IN_DATABASE"

{-# COMPLETE
  Log,
  DeleteFromDatabase,
  DeprecateInDatabase,
  DeleteBehavior'
  #-}
