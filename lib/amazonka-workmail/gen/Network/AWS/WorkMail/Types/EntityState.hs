-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkMail.Types.EntityState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkMail.Types.EntityState
  ( EntityState
      ( EntityState',
        Deleted,
        Disabled,
        Enabled
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype EntityState = EntityState' Lude.Text
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

pattern Deleted :: EntityState
pattern Deleted = EntityState' "DELETED"

pattern Disabled :: EntityState
pattern Disabled = EntityState' "DISABLED"

pattern Enabled :: EntityState
pattern Enabled = EntityState' "ENABLED"

{-# COMPLETE
  Deleted,
  Disabled,
  Enabled,
  EntityState'
  #-}
