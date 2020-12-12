{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.BurnInTeletextGridControl
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.BurnInTeletextGridControl
  ( BurnInTeletextGridControl
      ( BurnInTeletextGridControl',
        BITGCFixed,
        BITGCScaled
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Burn In Teletext Grid Control
newtype BurnInTeletextGridControl = BurnInTeletextGridControl' Lude.Text
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

pattern BITGCFixed :: BurnInTeletextGridControl
pattern BITGCFixed = BurnInTeletextGridControl' "FIXED"

pattern BITGCScaled :: BurnInTeletextGridControl
pattern BITGCScaled = BurnInTeletextGridControl' "SCALED"

{-# COMPLETE
  BITGCFixed,
  BITGCScaled,
  BurnInTeletextGridControl'
  #-}
