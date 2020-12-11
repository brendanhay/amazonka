-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.Types.LastUpdateStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lambda.Types.LastUpdateStatus
  ( LastUpdateStatus
      ( LastUpdateStatus',
        LUSFailed,
        LUSInProgress,
        LUSSuccessful
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype LastUpdateStatus = LastUpdateStatus' Lude.Text
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

pattern LUSFailed :: LastUpdateStatus
pattern LUSFailed = LastUpdateStatus' "Failed"

pattern LUSInProgress :: LastUpdateStatus
pattern LUSInProgress = LastUpdateStatus' "InProgress"

pattern LUSSuccessful :: LastUpdateStatus
pattern LUSSuccessful = LastUpdateStatus' "Successful"

{-# COMPLETE
  LUSFailed,
  LUSInProgress,
  LUSSuccessful,
  LastUpdateStatus'
  #-}
