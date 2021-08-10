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
-- Module      : Network.AWS.Shield.Types.AttackPropertyIdentifier
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Shield.Types.AttackPropertyIdentifier
  ( AttackPropertyIdentifier
      ( ..,
        AttackPropertyIdentifier_DESTINATION_URL,
        AttackPropertyIdentifier_REFERRER,
        AttackPropertyIdentifier_SOURCE_ASN,
        AttackPropertyIdentifier_SOURCE_COUNTRY,
        AttackPropertyIdentifier_SOURCE_IP_ADDRESS,
        AttackPropertyIdentifier_SOURCE_USER_AGENT,
        AttackPropertyIdentifier_WORDPRESS_PINGBACK_REFLECTOR,
        AttackPropertyIdentifier_WORDPRESS_PINGBACK_SOURCE
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype AttackPropertyIdentifier = AttackPropertyIdentifier'
  { fromAttackPropertyIdentifier ::
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

pattern AttackPropertyIdentifier_DESTINATION_URL :: AttackPropertyIdentifier
pattern AttackPropertyIdentifier_DESTINATION_URL = AttackPropertyIdentifier' "DESTINATION_URL"

pattern AttackPropertyIdentifier_REFERRER :: AttackPropertyIdentifier
pattern AttackPropertyIdentifier_REFERRER = AttackPropertyIdentifier' "REFERRER"

pattern AttackPropertyIdentifier_SOURCE_ASN :: AttackPropertyIdentifier
pattern AttackPropertyIdentifier_SOURCE_ASN = AttackPropertyIdentifier' "SOURCE_ASN"

pattern AttackPropertyIdentifier_SOURCE_COUNTRY :: AttackPropertyIdentifier
pattern AttackPropertyIdentifier_SOURCE_COUNTRY = AttackPropertyIdentifier' "SOURCE_COUNTRY"

pattern AttackPropertyIdentifier_SOURCE_IP_ADDRESS :: AttackPropertyIdentifier
pattern AttackPropertyIdentifier_SOURCE_IP_ADDRESS = AttackPropertyIdentifier' "SOURCE_IP_ADDRESS"

pattern AttackPropertyIdentifier_SOURCE_USER_AGENT :: AttackPropertyIdentifier
pattern AttackPropertyIdentifier_SOURCE_USER_AGENT = AttackPropertyIdentifier' "SOURCE_USER_AGENT"

pattern AttackPropertyIdentifier_WORDPRESS_PINGBACK_REFLECTOR :: AttackPropertyIdentifier
pattern AttackPropertyIdentifier_WORDPRESS_PINGBACK_REFLECTOR = AttackPropertyIdentifier' "WORDPRESS_PINGBACK_REFLECTOR"

pattern AttackPropertyIdentifier_WORDPRESS_PINGBACK_SOURCE :: AttackPropertyIdentifier
pattern AttackPropertyIdentifier_WORDPRESS_PINGBACK_SOURCE = AttackPropertyIdentifier' "WORDPRESS_PINGBACK_SOURCE"

{-# COMPLETE
  AttackPropertyIdentifier_DESTINATION_URL,
  AttackPropertyIdentifier_REFERRER,
  AttackPropertyIdentifier_SOURCE_ASN,
  AttackPropertyIdentifier_SOURCE_COUNTRY,
  AttackPropertyIdentifier_SOURCE_IP_ADDRESS,
  AttackPropertyIdentifier_SOURCE_USER_AGENT,
  AttackPropertyIdentifier_WORDPRESS_PINGBACK_REFLECTOR,
  AttackPropertyIdentifier_WORDPRESS_PINGBACK_SOURCE,
  AttackPropertyIdentifier'
  #-}
