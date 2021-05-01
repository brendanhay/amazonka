{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
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

import qualified Network.AWS.Prelude as Prelude

newtype PublishingStatus = PublishingStatus'
  { fromPublishingStatus ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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
