{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ServiceState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ServiceState
  ( ServiceState
      ( ServiceState',
        SSfPending,
        SSfAvailable,
        SSfDeleting,
        SSfDeleted,
        SSfFailed
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ServiceState = ServiceState' Lude.Text
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

pattern SSfPending :: ServiceState
pattern SSfPending = ServiceState' "Pending"

pattern SSfAvailable :: ServiceState
pattern SSfAvailable = ServiceState' "Available"

pattern SSfDeleting :: ServiceState
pattern SSfDeleting = ServiceState' "Deleting"

pattern SSfDeleted :: ServiceState
pattern SSfDeleted = ServiceState' "Deleted"

pattern SSfFailed :: ServiceState
pattern SSfFailed = ServiceState' "Failed"

{-# COMPLETE
  SSfPending,
  SSfAvailable,
  SSfDeleting,
  SSfDeleted,
  SSfFailed,
  ServiceState'
  #-}
