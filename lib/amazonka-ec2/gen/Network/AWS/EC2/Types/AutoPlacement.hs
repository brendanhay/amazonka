-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.AutoPlacement
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.AutoPlacement
  ( AutoPlacement
      ( AutoPlacement',
        APON,
        APOff
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype AutoPlacement = AutoPlacement' Lude.Text
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

pattern APON :: AutoPlacement
pattern APON = AutoPlacement' "on"

pattern APOff :: AutoPlacement
pattern APOff = AutoPlacement' "off"

{-# COMPLETE
  APON,
  APOff,
  AutoPlacement'
  #-}
