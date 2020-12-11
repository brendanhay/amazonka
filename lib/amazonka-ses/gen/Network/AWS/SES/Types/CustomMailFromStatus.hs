-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.Types.CustomMailFromStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SES.Types.CustomMailFromStatus
  ( CustomMailFromStatus
      ( CustomMailFromStatus',
        CMFSFailed,
        CMFSPending,
        CMFSSuccess,
        CMFSTemporaryFailure
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype CustomMailFromStatus = CustomMailFromStatus' Lude.Text
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

pattern CMFSFailed :: CustomMailFromStatus
pattern CMFSFailed = CustomMailFromStatus' "Failed"

pattern CMFSPending :: CustomMailFromStatus
pattern CMFSPending = CustomMailFromStatus' "Pending"

pattern CMFSSuccess :: CustomMailFromStatus
pattern CMFSSuccess = CustomMailFromStatus' "Success"

pattern CMFSTemporaryFailure :: CustomMailFromStatus
pattern CMFSTemporaryFailure = CustomMailFromStatus' "TemporaryFailure"

{-# COMPLETE
  CMFSFailed,
  CMFSPending,
  CMFSSuccess,
  CMFSTemporaryFailure,
  CustomMailFromStatus'
  #-}
