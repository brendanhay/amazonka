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
        PublishingStatusPendingVerification,
        PublishingStatusPublishing,
        PublishingStatusUnableToPublishFixDestinationProperty,
        PublishingStatusStopped,
        fromPublishingStatus
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype PublishingStatus = PublishingStatus'
  { fromPublishingStatus ::
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

pattern PublishingStatusPendingVerification :: PublishingStatus
pattern PublishingStatusPendingVerification = PublishingStatus' "PENDING_VERIFICATION"

pattern PublishingStatusPublishing :: PublishingStatus
pattern PublishingStatusPublishing = PublishingStatus' "PUBLISHING"

pattern PublishingStatusUnableToPublishFixDestinationProperty :: PublishingStatus
pattern PublishingStatusUnableToPublishFixDestinationProperty = PublishingStatus' "UNABLE_TO_PUBLISH_FIX_DESTINATION_PROPERTY"

pattern PublishingStatusStopped :: PublishingStatus
pattern PublishingStatusStopped = PublishingStatus' "STOPPED"

{-# COMPLETE
  PublishingStatusPendingVerification,
  PublishingStatusPublishing,
  PublishingStatusUnableToPublishFixDestinationProperty,
  PublishingStatusStopped,
  PublishingStatus'
  #-}
