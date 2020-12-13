{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.Types.DedicatedTenancySupportResultEnum
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkSpaces.Types.DedicatedTenancySupportResultEnum
  ( DedicatedTenancySupportResultEnum
      ( DedicatedTenancySupportResultEnum',
        DTSREEnabled,
        DTSREDisabled
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype DedicatedTenancySupportResultEnum = DedicatedTenancySupportResultEnum' Lude.Text
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

pattern DTSREEnabled :: DedicatedTenancySupportResultEnum
pattern DTSREEnabled = DedicatedTenancySupportResultEnum' "ENABLED"

pattern DTSREDisabled :: DedicatedTenancySupportResultEnum
pattern DTSREDisabled = DedicatedTenancySupportResultEnum' "DISABLED"

{-# COMPLETE
  DTSREEnabled,
  DTSREDisabled,
  DedicatedTenancySupportResultEnum'
  #-}
