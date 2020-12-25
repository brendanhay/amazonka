{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.ChildPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.ChildPolicy
  ( ChildPolicy
      ( ChildPolicy',
        ChildPolicyTerminate,
        ChildPolicyRequestCancel,
        ChildPolicyAbandon,
        fromChildPolicy
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype ChildPolicy = ChildPolicy' {fromChildPolicy :: Core.Text}
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern ChildPolicyTerminate :: ChildPolicy
pattern ChildPolicyTerminate = ChildPolicy' "TERMINATE"

pattern ChildPolicyRequestCancel :: ChildPolicy
pattern ChildPolicyRequestCancel = ChildPolicy' "REQUEST_CANCEL"

pattern ChildPolicyAbandon :: ChildPolicy
pattern ChildPolicyAbandon = ChildPolicy' "ABANDON"

{-# COMPLETE
  ChildPolicyTerminate,
  ChildPolicyRequestCancel,
  ChildPolicyAbandon,
  ChildPolicy'
  #-}
