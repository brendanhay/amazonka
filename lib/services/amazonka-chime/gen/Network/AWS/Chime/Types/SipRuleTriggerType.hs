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
-- Module      : Network.AWS.Chime.Types.SipRuleTriggerType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Chime.Types.SipRuleTriggerType
  ( SipRuleTriggerType
      ( ..,
        SipRuleTriggerType_RequestUriHostname,
        SipRuleTriggerType_ToPhoneNumber
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype SipRuleTriggerType = SipRuleTriggerType'
  { fromSipRuleTriggerType ::
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

pattern SipRuleTriggerType_RequestUriHostname :: SipRuleTriggerType
pattern SipRuleTriggerType_RequestUriHostname = SipRuleTriggerType' "RequestUriHostname"

pattern SipRuleTriggerType_ToPhoneNumber :: SipRuleTriggerType
pattern SipRuleTriggerType_ToPhoneNumber = SipRuleTriggerType' "ToPhoneNumber"

{-# COMPLETE
  SipRuleTriggerType_RequestUriHostname,
  SipRuleTriggerType_ToPhoneNumber,
  SipRuleTriggerType'
  #-}
