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
-- Module      : Amazonka.GuardDuty.Types.PublishingStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.PublishingStatus
  ( PublishingStatus
      ( ..,
        PublishingStatus_PENDING_VERIFICATION,
        PublishingStatus_PUBLISHING,
        PublishingStatus_STOPPED,
        PublishingStatus_UNABLE_TO_PUBLISH_FIX_DESTINATION_PROPERTY
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype PublishingStatus = PublishingStatus'
  { fromPublishingStatus ::
      Data.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
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
