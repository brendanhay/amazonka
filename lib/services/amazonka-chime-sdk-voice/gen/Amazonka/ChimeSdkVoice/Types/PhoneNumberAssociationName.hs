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
-- Module      : Amazonka.ChimeSdkVoice.Types.PhoneNumberAssociationName
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSdkVoice.Types.PhoneNumberAssociationName
  ( PhoneNumberAssociationName
      ( ..,
        PhoneNumberAssociationName_SipRuleId,
        PhoneNumberAssociationName_VoiceConnectorGroupId,
        PhoneNumberAssociationName_VoiceConnectorId
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype PhoneNumberAssociationName = PhoneNumberAssociationName'
  { fromPhoneNumberAssociationName ::
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

pattern PhoneNumberAssociationName_SipRuleId :: PhoneNumberAssociationName
pattern PhoneNumberAssociationName_SipRuleId = PhoneNumberAssociationName' "SipRuleId"

pattern PhoneNumberAssociationName_VoiceConnectorGroupId :: PhoneNumberAssociationName
pattern PhoneNumberAssociationName_VoiceConnectorGroupId = PhoneNumberAssociationName' "VoiceConnectorGroupId"

pattern PhoneNumberAssociationName_VoiceConnectorId :: PhoneNumberAssociationName
pattern PhoneNumberAssociationName_VoiceConnectorId = PhoneNumberAssociationName' "VoiceConnectorId"

{-# COMPLETE
  PhoneNumberAssociationName_SipRuleId,
  PhoneNumberAssociationName_VoiceConnectorGroupId,
  PhoneNumberAssociationName_VoiceConnectorId,
  PhoneNumberAssociationName'
  #-}
