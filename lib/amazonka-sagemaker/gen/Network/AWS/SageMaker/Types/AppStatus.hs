-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.AppStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.AppStatus
  ( AppStatus
      ( AppStatus',
        ASDeleted,
        ASDeleting,
        ASFailed,
        ASInService,
        ASPending
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype AppStatus = AppStatus' Lude.Text
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

pattern ASDeleted :: AppStatus
pattern ASDeleted = AppStatus' "Deleted"

pattern ASDeleting :: AppStatus
pattern ASDeleting = AppStatus' "Deleting"

pattern ASFailed :: AppStatus
pattern ASFailed = AppStatus' "Failed"

pattern ASInService :: AppStatus
pattern ASInService = AppStatus' "InService"

pattern ASPending :: AppStatus
pattern ASPending = AppStatus' "Pending"

{-# COMPLETE
  ASDeleted,
  ASDeleting,
  ASFailed,
  ASInService,
  ASPending,
  AppStatus'
  #-}
