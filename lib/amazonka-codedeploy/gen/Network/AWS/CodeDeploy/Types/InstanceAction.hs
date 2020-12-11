-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.InstanceAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.InstanceAction
  ( InstanceAction
      ( InstanceAction',
        KeepAlive,
        Terminate
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype InstanceAction = InstanceAction' Lude.Text
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

pattern KeepAlive :: InstanceAction
pattern KeepAlive = InstanceAction' "KEEP_ALIVE"

pattern Terminate :: InstanceAction
pattern Terminate = InstanceAction' "TERMINATE"

{-# COMPLETE
  KeepAlive,
  Terminate,
  InstanceAction'
  #-}
