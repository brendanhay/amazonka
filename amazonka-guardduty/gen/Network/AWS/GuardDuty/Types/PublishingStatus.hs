{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.PublishingStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.PublishingStatus
  ( PublishingStatus
      ( ..,
        PublishingStatus_PENDING_VERIFICATION,
        PublishingStatus_PUBLISHING,
        PublishingStatus_STOPPED,
        PublishingStatus_UNABLE_TO_PUBLISH_FIX_DESTINATION_PROPERTY
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype PublishingStatus = PublishingStatus'
  { fromPublishingStatus ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
    )

pattern PublishingStatus_PENDING_VERIFICATION :: PublishingStatus
pattern PublishingStatus_PENDING_VERIFICATION = PublishingStatus' "PENDING_VERIFICATION"

pattern PublishingStatus_PUBLISHING :: PublishingStatus
pattern PublishingStatus_PUBLISHING = PublishingStatus' "PUBLISHING"

pattern PublishingStatus_STOPPED :: PublishingStatus
pattern PublishingStatus_STOPPED = PublishingStatus' "STOPPED"

pattern PublishingStatus_UNABLE_TO_PUBLISH_FIX_DESTINATION_PROPERTY :: PublishingStatus
pattern PublishingStatus_UNABLE_TO_PUBLISH_FIX_DESTINATION_PROPERTY = PublishingStatus' "UNABLE_TO_PUBLISH_FIX_DESTINATION_PROPERTY"

{-# COMPLETE
  PublishingStatus_PENDING_VERIFICATION,
  PublishingStatus_PUBLISHING,
  PublishingStatus_STOPPED,
  PublishingStatus_UNABLE_TO_PUBLISH_FIX_DESTINATION_PROPERTY,
  PublishingStatus'
  #-}
