{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.TtmlDestinationStyleControl
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.TtmlDestinationStyleControl
  ( TtmlDestinationStyleControl
      ( TtmlDestinationStyleControl',
        TDSCPassthrough,
        TDSCUseConfigured
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Ttml Destination Style Control
newtype TtmlDestinationStyleControl = TtmlDestinationStyleControl' Lude.Text
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

pattern TDSCPassthrough :: TtmlDestinationStyleControl
pattern TDSCPassthrough = TtmlDestinationStyleControl' "PASSTHROUGH"

pattern TDSCUseConfigured :: TtmlDestinationStyleControl
pattern TDSCUseConfigured = TtmlDestinationStyleControl' "USE_CONFIGURED"

{-# COMPLETE
  TDSCPassthrough,
  TDSCUseConfigured,
  TtmlDestinationStyleControl'
  #-}
