{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.ConferenceProviderType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.ConferenceProviderType
  ( ConferenceProviderType
      ( ConferenceProviderType',
        ConferenceProviderTypeChime,
        ConferenceProviderTypeBluejeans,
        ConferenceProviderTypeFuze,
        ConferenceProviderTypeGoogleHangouts,
        ConferenceProviderTypePolycom,
        ConferenceProviderTypeRingcentral,
        ConferenceProviderTypeSkypeForBusiness,
        ConferenceProviderTypeWebex,
        ConferenceProviderTypeZoom,
        ConferenceProviderTypeCustom,
        fromConferenceProviderType
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype ConferenceProviderType = ConferenceProviderType'
  { fromConferenceProviderType ::
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

pattern ConferenceProviderTypeChime :: ConferenceProviderType
pattern ConferenceProviderTypeChime = ConferenceProviderType' "CHIME"

pattern ConferenceProviderTypeBluejeans :: ConferenceProviderType
pattern ConferenceProviderTypeBluejeans = ConferenceProviderType' "BLUEJEANS"

pattern ConferenceProviderTypeFuze :: ConferenceProviderType
pattern ConferenceProviderTypeFuze = ConferenceProviderType' "FUZE"

pattern ConferenceProviderTypeGoogleHangouts :: ConferenceProviderType
pattern ConferenceProviderTypeGoogleHangouts = ConferenceProviderType' "GOOGLE_HANGOUTS"

pattern ConferenceProviderTypePolycom :: ConferenceProviderType
pattern ConferenceProviderTypePolycom = ConferenceProviderType' "POLYCOM"

pattern ConferenceProviderTypeRingcentral :: ConferenceProviderType
pattern ConferenceProviderTypeRingcentral = ConferenceProviderType' "RINGCENTRAL"

pattern ConferenceProviderTypeSkypeForBusiness :: ConferenceProviderType
pattern ConferenceProviderTypeSkypeForBusiness = ConferenceProviderType' "SKYPE_FOR_BUSINESS"

pattern ConferenceProviderTypeWebex :: ConferenceProviderType
pattern ConferenceProviderTypeWebex = ConferenceProviderType' "WEBEX"

pattern ConferenceProviderTypeZoom :: ConferenceProviderType
pattern ConferenceProviderTypeZoom = ConferenceProviderType' "ZOOM"

pattern ConferenceProviderTypeCustom :: ConferenceProviderType
pattern ConferenceProviderTypeCustom = ConferenceProviderType' "CUSTOM"

{-# COMPLETE
  ConferenceProviderTypeChime,
  ConferenceProviderTypeBluejeans,
  ConferenceProviderTypeFuze,
  ConferenceProviderTypeGoogleHangouts,
  ConferenceProviderTypePolycom,
  ConferenceProviderTypeRingcentral,
  ConferenceProviderTypeSkypeForBusiness,
  ConferenceProviderTypeWebex,
  ConferenceProviderTypeZoom,
  ConferenceProviderTypeCustom,
  ConferenceProviderType'
  #-}
