{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.EnrollmentStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.EnrollmentStatus
  ( EnrollmentStatus
      ( EnrollmentStatus',
        ESDeregistering,
        ESDisassociating,
        ESInitialized,
        ESPending,
        ESRegistered
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype EnrollmentStatus = EnrollmentStatus' Lude.Text
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

pattern ESDeregistering :: EnrollmentStatus
pattern ESDeregistering = EnrollmentStatus' "DEREGISTERING"

pattern ESDisassociating :: EnrollmentStatus
pattern ESDisassociating = EnrollmentStatus' "DISASSOCIATING"

pattern ESInitialized :: EnrollmentStatus
pattern ESInitialized = EnrollmentStatus' "INITIALIZED"

pattern ESPending :: EnrollmentStatus
pattern ESPending = EnrollmentStatus' "PENDING"

pattern ESRegistered :: EnrollmentStatus
pattern ESRegistered = EnrollmentStatus' "REGISTERED"

{-# COMPLETE
  ESDeregistering,
  ESDisassociating,
  ESInitialized,
  ESPending,
  ESRegistered,
  EnrollmentStatus'
  #-}
