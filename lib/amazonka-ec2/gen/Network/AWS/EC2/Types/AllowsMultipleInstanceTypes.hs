{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.AllowsMultipleInstanceTypes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.AllowsMultipleInstanceTypes
  ( AllowsMultipleInstanceTypes
      ( AllowsMultipleInstanceTypes',
        ON,
        Off
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype AllowsMultipleInstanceTypes = AllowsMultipleInstanceTypes' Lude.Text
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

pattern ON :: AllowsMultipleInstanceTypes
pattern ON = AllowsMultipleInstanceTypes' "on"

pattern Off :: AllowsMultipleInstanceTypes
pattern Off = AllowsMultipleInstanceTypes' "off"

{-# COMPLETE
  ON,
  Off,
  AllowsMultipleInstanceTypes'
  #-}
