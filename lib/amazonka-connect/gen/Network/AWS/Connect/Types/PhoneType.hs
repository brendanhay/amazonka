{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.PhoneType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Connect.Types.PhoneType
  ( PhoneType
    ( PhoneType'
    , PhoneTypeSoftPhone
    , PhoneTypeDeskPhone
    , fromPhoneType
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype PhoneType = PhoneType'{fromPhoneType :: Core.Text}
                      deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                      Core.Generic)
                      deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                        Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                        Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                        Core.FromText, Core.ToByteString, Core.ToQuery,
                                        Core.ToHeader)

pattern PhoneTypeSoftPhone :: PhoneType
pattern PhoneTypeSoftPhone = PhoneType' "SOFT_PHONE"

pattern PhoneTypeDeskPhone :: PhoneType
pattern PhoneTypeDeskPhone = PhoneType' "DESK_PHONE"

{-# COMPLETE 
  PhoneTypeSoftPhone,

  PhoneTypeDeskPhone,
  PhoneType'
  #-}
