{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoSync.Types.Platform
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoSync.Types.Platform
  ( Platform
      ( Platform',
        APNS,
        APNSSandbox,
        GCM,
        ADM
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype Platform = Platform' Lude.Text
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

pattern APNS :: Platform
pattern APNS = Platform' "APNS"

pattern APNSSandbox :: Platform
pattern APNSSandbox = Platform' "APNS_SANDBOX"

pattern GCM :: Platform
pattern GCM = Platform' "GCM"

pattern ADM :: Platform
pattern ADM = Platform' "ADM"

{-# COMPLETE
  APNS,
  APNSSandbox,
  GCM,
  ADM,
  Platform'
  #-}
