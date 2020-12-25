{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.Types.ChangeTokenStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAFRegional.Types.ChangeTokenStatus
  ( ChangeTokenStatus
      ( ChangeTokenStatus',
        ChangeTokenStatusProvisioned,
        ChangeTokenStatusPending,
        ChangeTokenStatusInsync,
        fromChangeTokenStatus
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype ChangeTokenStatus = ChangeTokenStatus'
  { fromChangeTokenStatus ::
      Core.Text
  }
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

pattern ChangeTokenStatusProvisioned :: ChangeTokenStatus
pattern ChangeTokenStatusProvisioned = ChangeTokenStatus' "PROVISIONED"

pattern ChangeTokenStatusPending :: ChangeTokenStatus
pattern ChangeTokenStatusPending = ChangeTokenStatus' "PENDING"

pattern ChangeTokenStatusInsync :: ChangeTokenStatus
pattern ChangeTokenStatusInsync = ChangeTokenStatus' "INSYNC"

{-# COMPLETE
  ChangeTokenStatusProvisioned,
  ChangeTokenStatusPending,
  ChangeTokenStatusInsync,
  ChangeTokenStatus'
  #-}
