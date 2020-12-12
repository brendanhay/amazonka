{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.AutomaticFailoverStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.AutomaticFailoverStatus
  ( AutomaticFailoverStatus
      ( AutomaticFailoverStatus',
        AFSDisabled,
        AFSDisabling,
        AFSEnabled,
        AFSEnabling
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype AutomaticFailoverStatus = AutomaticFailoverStatus' Lude.Text
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

pattern AFSDisabled :: AutomaticFailoverStatus
pattern AFSDisabled = AutomaticFailoverStatus' "disabled"

pattern AFSDisabling :: AutomaticFailoverStatus
pattern AFSDisabling = AutomaticFailoverStatus' "disabling"

pattern AFSEnabled :: AutomaticFailoverStatus
pattern AFSEnabled = AutomaticFailoverStatus' "enabled"

pattern AFSEnabling :: AutomaticFailoverStatus
pattern AFSEnabling = AutomaticFailoverStatus' "enabling"

{-# COMPLETE
  AFSDisabled,
  AFSDisabling,
  AFSEnabled,
  AFSEnabling,
  AutomaticFailoverStatus'
  #-}
