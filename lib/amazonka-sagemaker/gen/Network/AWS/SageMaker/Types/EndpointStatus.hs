-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.EndpointStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.EndpointStatus
  ( EndpointStatus
      ( EndpointStatus',
        ESCreating,
        ESDeleting,
        ESFailed,
        ESInService,
        ESOutOfService,
        ESRollingBack,
        ESSystemUpdating,
        ESUpdating
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype EndpointStatus = EndpointStatus' Lude.Text
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

pattern ESCreating :: EndpointStatus
pattern ESCreating = EndpointStatus' "Creating"

pattern ESDeleting :: EndpointStatus
pattern ESDeleting = EndpointStatus' "Deleting"

pattern ESFailed :: EndpointStatus
pattern ESFailed = EndpointStatus' "Failed"

pattern ESInService :: EndpointStatus
pattern ESInService = EndpointStatus' "InService"

pattern ESOutOfService :: EndpointStatus
pattern ESOutOfService = EndpointStatus' "OutOfService"

pattern ESRollingBack :: EndpointStatus
pattern ESRollingBack = EndpointStatus' "RollingBack"

pattern ESSystemUpdating :: EndpointStatus
pattern ESSystemUpdating = EndpointStatus' "SystemUpdating"

pattern ESUpdating :: EndpointStatus
pattern ESUpdating = EndpointStatus' "Updating"

{-# COMPLETE
  ESCreating,
  ESDeleting,
  ESFailed,
  ESInService,
  ESOutOfService,
  ESRollingBack,
  ESSystemUpdating,
  ESUpdating,
  EndpointStatus'
  #-}
