-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.Types.JQState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Batch.Types.JQState
  ( JQState
      ( JQState',
        JQSDisabled,
        JQSEnabled
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype JQState = JQState' Lude.Text
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

pattern JQSDisabled :: JQState
pattern JQSDisabled = JQState' "DISABLED"

pattern JQSEnabled :: JQState
pattern JQSEnabled = JQState' "ENABLED"

{-# COMPLETE
  JQSDisabled,
  JQSEnabled,
  JQState'
  #-}
