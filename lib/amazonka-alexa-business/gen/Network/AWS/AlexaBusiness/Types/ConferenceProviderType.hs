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
        Chime,
        Bluejeans,
        Fuze,
        GoogleHangouts,
        Polycom,
        Ringcentral,
        SkypeForBusiness,
        Webex,
        Zoom,
        Custom
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ConferenceProviderType = ConferenceProviderType' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern Chime :: ConferenceProviderType
pattern Chime = ConferenceProviderType' "CHIME"

pattern Bluejeans :: ConferenceProviderType
pattern Bluejeans = ConferenceProviderType' "BLUEJEANS"

pattern Fuze :: ConferenceProviderType
pattern Fuze = ConferenceProviderType' "FUZE"

pattern GoogleHangouts :: ConferenceProviderType
pattern GoogleHangouts = ConferenceProviderType' "GOOGLE_HANGOUTS"

pattern Polycom :: ConferenceProviderType
pattern Polycom = ConferenceProviderType' "POLYCOM"

pattern Ringcentral :: ConferenceProviderType
pattern Ringcentral = ConferenceProviderType' "RINGCENTRAL"

pattern SkypeForBusiness :: ConferenceProviderType
pattern SkypeForBusiness = ConferenceProviderType' "SKYPE_FOR_BUSINESS"

pattern Webex :: ConferenceProviderType
pattern Webex = ConferenceProviderType' "WEBEX"

pattern Zoom :: ConferenceProviderType
pattern Zoom = ConferenceProviderType' "ZOOM"

pattern Custom :: ConferenceProviderType
pattern Custom = ConferenceProviderType' "CUSTOM"

{-# COMPLETE
  Chime,
  Bluejeans,
  Fuze,
  GoogleHangouts,
  Polycom,
  Ringcentral,
  SkypeForBusiness,
  Webex,
  Zoom,
  Custom,
  ConferenceProviderType'
  #-}
