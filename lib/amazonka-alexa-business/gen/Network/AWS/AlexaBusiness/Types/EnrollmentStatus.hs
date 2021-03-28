{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.EnrollmentStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AlexaBusiness.Types.EnrollmentStatus
  ( EnrollmentStatus
    ( EnrollmentStatus'
    , EnrollmentStatusInitialized
    , EnrollmentStatusPending
    , EnrollmentStatusRegistered
    , EnrollmentStatusDisassociating
    , EnrollmentStatusDeregistering
    , fromEnrollmentStatus
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype EnrollmentStatus = EnrollmentStatus'{fromEnrollmentStatus
                                             :: Core.Text}
                             deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                             Core.Generic)
                             deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                               Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                               Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                               Core.FromText, Core.ToByteString, Core.ToQuery,
                                               Core.ToHeader)

pattern EnrollmentStatusInitialized :: EnrollmentStatus
pattern EnrollmentStatusInitialized = EnrollmentStatus' "INITIALIZED"

pattern EnrollmentStatusPending :: EnrollmentStatus
pattern EnrollmentStatusPending = EnrollmentStatus' "PENDING"

pattern EnrollmentStatusRegistered :: EnrollmentStatus
pattern EnrollmentStatusRegistered = EnrollmentStatus' "REGISTERED"

pattern EnrollmentStatusDisassociating :: EnrollmentStatus
pattern EnrollmentStatusDisassociating = EnrollmentStatus' "DISASSOCIATING"

pattern EnrollmentStatusDeregistering :: EnrollmentStatus
pattern EnrollmentStatusDeregistering = EnrollmentStatus' "DEREGISTERING"

{-# COMPLETE 
  EnrollmentStatusInitialized,

  EnrollmentStatusPending,

  EnrollmentStatusRegistered,

  EnrollmentStatusDisassociating,

  EnrollmentStatusDeregistering,
  EnrollmentStatus'
  #-}
