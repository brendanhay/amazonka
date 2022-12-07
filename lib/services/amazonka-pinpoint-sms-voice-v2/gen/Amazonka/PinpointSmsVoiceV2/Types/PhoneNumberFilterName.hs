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
-- Module      : Amazonka.PinpointSmsVoiceV2.Types.PhoneNumberFilterName
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PinpointSmsVoiceV2.Types.PhoneNumberFilterName
  ( PhoneNumberFilterName
      ( ..,
        PhoneNumberFilterName_Deletion_protection_enabled,
        PhoneNumberFilterName_Iso_country_code,
        PhoneNumberFilterName_Message_type,
        PhoneNumberFilterName_Number_capability,
        PhoneNumberFilterName_Number_type,
        PhoneNumberFilterName_Opt_out_list_name,
        PhoneNumberFilterName_Self_managed_opt_outs_enabled,
        PhoneNumberFilterName_Status,
        PhoneNumberFilterName_Two_way_enabled
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype PhoneNumberFilterName = PhoneNumberFilterName'
  { fromPhoneNumberFilterName ::
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

pattern PhoneNumberFilterName_Deletion_protection_enabled :: PhoneNumberFilterName
pattern PhoneNumberFilterName_Deletion_protection_enabled = PhoneNumberFilterName' "deletion-protection-enabled"

pattern PhoneNumberFilterName_Iso_country_code :: PhoneNumberFilterName
pattern PhoneNumberFilterName_Iso_country_code = PhoneNumberFilterName' "iso-country-code"

pattern PhoneNumberFilterName_Message_type :: PhoneNumberFilterName
pattern PhoneNumberFilterName_Message_type = PhoneNumberFilterName' "message-type"

pattern PhoneNumberFilterName_Number_capability :: PhoneNumberFilterName
pattern PhoneNumberFilterName_Number_capability = PhoneNumberFilterName' "number-capability"

pattern PhoneNumberFilterName_Number_type :: PhoneNumberFilterName
pattern PhoneNumberFilterName_Number_type = PhoneNumberFilterName' "number-type"

pattern PhoneNumberFilterName_Opt_out_list_name :: PhoneNumberFilterName
pattern PhoneNumberFilterName_Opt_out_list_name = PhoneNumberFilterName' "opt-out-list-name"

pattern PhoneNumberFilterName_Self_managed_opt_outs_enabled :: PhoneNumberFilterName
pattern PhoneNumberFilterName_Self_managed_opt_outs_enabled = PhoneNumberFilterName' "self-managed-opt-outs-enabled"

pattern PhoneNumberFilterName_Status :: PhoneNumberFilterName
pattern PhoneNumberFilterName_Status = PhoneNumberFilterName' "status"

pattern PhoneNumberFilterName_Two_way_enabled :: PhoneNumberFilterName
pattern PhoneNumberFilterName_Two_way_enabled = PhoneNumberFilterName' "two-way-enabled"

{-# COMPLETE
  PhoneNumberFilterName_Deletion_protection_enabled,
  PhoneNumberFilterName_Iso_country_code,
  PhoneNumberFilterName_Message_type,
  PhoneNumberFilterName_Number_capability,
  PhoneNumberFilterName_Number_type,
  PhoneNumberFilterName_Opt_out_list_name,
  PhoneNumberFilterName_Self_managed_opt_outs_enabled,
  PhoneNumberFilterName_Status,
  PhoneNumberFilterName_Two_way_enabled,
  PhoneNumberFilterName'
  #-}
