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
-- Module      : Network.AWS.AlexaBusiness.Types.ConferenceProviderType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.ConferenceProviderType
  ( ConferenceProviderType
      ( ..,
        ConferenceProviderType_BLUEJEANS,
        ConferenceProviderType_CHIME,
        ConferenceProviderType_CUSTOM,
        ConferenceProviderType_FUZE,
        ConferenceProviderType_GOOGLE_HANGOUTS,
        ConferenceProviderType_POLYCOM,
        ConferenceProviderType_RINGCENTRAL,
        ConferenceProviderType_SKYPE_FOR_BUSINESS,
        ConferenceProviderType_WEBEX,
        ConferenceProviderType_ZOOM
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype ConferenceProviderType = ConferenceProviderType'
  { fromConferenceProviderType ::
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

pattern ConferenceProviderType_BLUEJEANS :: ConferenceProviderType
pattern ConferenceProviderType_BLUEJEANS = ConferenceProviderType' "BLUEJEANS"

pattern ConferenceProviderType_CHIME :: ConferenceProviderType
pattern ConferenceProviderType_CHIME = ConferenceProviderType' "CHIME"

pattern ConferenceProviderType_CUSTOM :: ConferenceProviderType
pattern ConferenceProviderType_CUSTOM = ConferenceProviderType' "CUSTOM"

pattern ConferenceProviderType_FUZE :: ConferenceProviderType
pattern ConferenceProviderType_FUZE = ConferenceProviderType' "FUZE"

pattern ConferenceProviderType_GOOGLE_HANGOUTS :: ConferenceProviderType
pattern ConferenceProviderType_GOOGLE_HANGOUTS = ConferenceProviderType' "GOOGLE_HANGOUTS"

pattern ConferenceProviderType_POLYCOM :: ConferenceProviderType
pattern ConferenceProviderType_POLYCOM = ConferenceProviderType' "POLYCOM"

pattern ConferenceProviderType_RINGCENTRAL :: ConferenceProviderType
pattern ConferenceProviderType_RINGCENTRAL = ConferenceProviderType' "RINGCENTRAL"

pattern ConferenceProviderType_SKYPE_FOR_BUSINESS :: ConferenceProviderType
pattern ConferenceProviderType_SKYPE_FOR_BUSINESS = ConferenceProviderType' "SKYPE_FOR_BUSINESS"

pattern ConferenceProviderType_WEBEX :: ConferenceProviderType
pattern ConferenceProviderType_WEBEX = ConferenceProviderType' "WEBEX"

pattern ConferenceProviderType_ZOOM :: ConferenceProviderType
pattern ConferenceProviderType_ZOOM = ConferenceProviderType' "ZOOM"

{-# COMPLETE
  ConferenceProviderType_BLUEJEANS,
  ConferenceProviderType_CHIME,
  ConferenceProviderType_CUSTOM,
  ConferenceProviderType_FUZE,
  ConferenceProviderType_GOOGLE_HANGOUTS,
  ConferenceProviderType_POLYCOM,
  ConferenceProviderType_RINGCENTRAL,
  ConferenceProviderType_SKYPE_FOR_BUSINESS,
  ConferenceProviderType_WEBEX,
  ConferenceProviderType_ZOOM,
  ConferenceProviderType'
  #-}
