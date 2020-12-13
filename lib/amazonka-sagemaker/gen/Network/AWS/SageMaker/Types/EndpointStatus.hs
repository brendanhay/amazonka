{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
        ESOutOfService,
        ESCreating,
        ESUpdating,
        ESSystemUpdating,
        ESRollingBack,
        ESInService,
        ESDeleting,
        ESFailed
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

pattern ESOutOfService :: EndpointStatus
pattern ESOutOfService = EndpointStatus' "OutOfService"

pattern ESCreating :: EndpointStatus
pattern ESCreating = EndpointStatus' "Creating"

pattern ESUpdating :: EndpointStatus
pattern ESUpdating = EndpointStatus' "Updating"

pattern ESSystemUpdating :: EndpointStatus
pattern ESSystemUpdating = EndpointStatus' "SystemUpdating"

pattern ESRollingBack :: EndpointStatus
pattern ESRollingBack = EndpointStatus' "RollingBack"

pattern ESInService :: EndpointStatus
pattern ESInService = EndpointStatus' "InService"

pattern ESDeleting :: EndpointStatus
pattern ESDeleting = EndpointStatus' "Deleting"

pattern ESFailed :: EndpointStatus
pattern ESFailed = EndpointStatus' "Failed"

{-# COMPLETE
  ESOutOfService,
  ESCreating,
  ESUpdating,
  ESSystemUpdating,
  ESRollingBack,
  ESInService,
  ESDeleting,
  ESFailed,
  EndpointStatus'
  #-}
