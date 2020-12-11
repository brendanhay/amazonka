-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.M2tsAribCaptionsPidControl
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.M2tsAribCaptionsPidControl
  ( M2tsAribCaptionsPidControl
      ( M2tsAribCaptionsPidControl',
        MACPCAuto,
        MACPCUseConfigured
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | M2ts Arib Captions Pid Control
newtype M2tsAribCaptionsPidControl = M2tsAribCaptionsPidControl' Lude.Text
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

pattern MACPCAuto :: M2tsAribCaptionsPidControl
pattern MACPCAuto = M2tsAribCaptionsPidControl' "AUTO"

pattern MACPCUseConfigured :: M2tsAribCaptionsPidControl
pattern MACPCUseConfigured = M2tsAribCaptionsPidControl' "USE_CONFIGURED"

{-# COMPLETE
  MACPCAuto,
  MACPCUseConfigured,
  M2tsAribCaptionsPidControl'
  #-}
