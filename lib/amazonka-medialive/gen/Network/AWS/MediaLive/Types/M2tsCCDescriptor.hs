{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.M2tsCCDescriptor
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.M2tsCCDescriptor
  ( M2tsCCDescriptor
      ( M2tsCCDescriptor',
        MCCDDisabled,
        MCCDEnabled
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | M2ts Cc Descriptor
newtype M2tsCCDescriptor = M2tsCCDescriptor' Lude.Text
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

pattern MCCDDisabled :: M2tsCCDescriptor
pattern MCCDDisabled = M2tsCCDescriptor' "DISABLED"

pattern MCCDEnabled :: M2tsCCDescriptor
pattern MCCDEnabled = M2tsCCDescriptor' "ENABLED"

{-# COMPLETE
  MCCDDisabled,
  MCCDEnabled,
  M2tsCCDescriptor'
  #-}
