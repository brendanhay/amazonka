{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.PublishingStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.PublishingStatus
  ( PublishingStatus
      ( PublishingStatus',
        PendingVerification,
        Publishing,
        UnableToPublishFixDestinationProperty,
        Stopped
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype PublishingStatus = PublishingStatus' Lude.Text
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

pattern PendingVerification :: PublishingStatus
pattern PendingVerification = PublishingStatus' "PENDING_VERIFICATION"

pattern Publishing :: PublishingStatus
pattern Publishing = PublishingStatus' "PUBLISHING"

pattern UnableToPublishFixDestinationProperty :: PublishingStatus
pattern UnableToPublishFixDestinationProperty = PublishingStatus' "UNABLE_TO_PUBLISH_FIX_DESTINATION_PROPERTY"

pattern Stopped :: PublishingStatus
pattern Stopped = PublishingStatus' "STOPPED"

{-# COMPLETE
  PendingVerification,
  Publishing,
  UnableToPublishFixDestinationProperty,
  Stopped,
  PublishingStatus'
  #-}
