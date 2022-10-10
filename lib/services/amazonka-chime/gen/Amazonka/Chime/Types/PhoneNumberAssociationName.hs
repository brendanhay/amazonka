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
-- Module      : Amazonka.Chime.Types.PhoneNumberAssociationName
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Chime.Types.PhoneNumberAssociationName
  ( PhoneNumberAssociationName
      ( ..,
        PhoneNumberAssociationName_AccountId,
        PhoneNumberAssociationName_SipRuleId,
        PhoneNumberAssociationName_UserId,
        PhoneNumberAssociationName_VoiceConnectorGroupId,
        PhoneNumberAssociationName_VoiceConnectorId
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype PhoneNumberAssociationName = PhoneNumberAssociationName'
  { fromPhoneNumberAssociationName ::
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

pattern PhoneNumberAssociationName_AccountId :: PhoneNumberAssociationName
pattern PhoneNumberAssociationName_AccountId = PhoneNumberAssociationName' "AccountId"

pattern PhoneNumberAssociationName_SipRuleId :: PhoneNumberAssociationName
pattern PhoneNumberAssociationName_SipRuleId = PhoneNumberAssociationName' "SipRuleId"

pattern PhoneNumberAssociationName_UserId :: PhoneNumberAssociationName
pattern PhoneNumberAssociationName_UserId = PhoneNumberAssociationName' "UserId"

pattern PhoneNumberAssociationName_VoiceConnectorGroupId :: PhoneNumberAssociationName
pattern PhoneNumberAssociationName_VoiceConnectorGroupId = PhoneNumberAssociationName' "VoiceConnectorGroupId"

pattern PhoneNumberAssociationName_VoiceConnectorId :: PhoneNumberAssociationName
pattern PhoneNumberAssociationName_VoiceConnectorId = PhoneNumberAssociationName' "VoiceConnectorId"

{-# COMPLETE
  PhoneNumberAssociationName_AccountId,
  PhoneNumberAssociationName_SipRuleId,
  PhoneNumberAssociationName_UserId,
  PhoneNumberAssociationName_VoiceConnectorGroupId,
  PhoneNumberAssociationName_VoiceConnectorId,
  PhoneNumberAssociationName'
  #-}
