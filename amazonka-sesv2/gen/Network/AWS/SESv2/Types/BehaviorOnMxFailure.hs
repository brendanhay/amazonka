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
-- Module      : Network.AWS.SESv2.Types.BehaviorOnMxFailure
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SESv2.Types.BehaviorOnMxFailure
  ( BehaviorOnMxFailure
      ( ..,
        BehaviorOnMxFailure_REJECT_MESSAGE,
        BehaviorOnMxFailure_USE_DEFAULT_VALUE
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

-- | The action that you want to take if the required MX record can\'t be
-- found when you send an email. When you set this value to
-- @UseDefaultValue@, the mail is sent using /amazonses.com/ as the MAIL
-- FROM domain. When you set this value to @RejectMessage@, the Amazon SES
-- API v2 returns a @MailFromDomainNotVerified@ error, and doesn\'t attempt
-- to deliver the email.
--
-- These behaviors are taken when the custom MAIL FROM domain configuration
-- is in the @Pending@, @Failed@, and @TemporaryFailure@ states.
newtype BehaviorOnMxFailure = BehaviorOnMxFailure'
  { fromBehaviorOnMxFailure ::
      Core.Text
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

pattern BehaviorOnMxFailure_REJECT_MESSAGE :: BehaviorOnMxFailure
pattern BehaviorOnMxFailure_REJECT_MESSAGE = BehaviorOnMxFailure' "REJECT_MESSAGE"

pattern BehaviorOnMxFailure_USE_DEFAULT_VALUE :: BehaviorOnMxFailure
pattern BehaviorOnMxFailure_USE_DEFAULT_VALUE = BehaviorOnMxFailure' "USE_DEFAULT_VALUE"

{-# COMPLETE
  BehaviorOnMxFailure_REJECT_MESSAGE,
  BehaviorOnMxFailure_USE_DEFAULT_VALUE,
  BehaviorOnMxFailure'
  #-}
