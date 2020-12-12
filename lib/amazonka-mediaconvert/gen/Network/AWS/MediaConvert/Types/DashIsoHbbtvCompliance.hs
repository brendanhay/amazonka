{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.DashIsoHbbtvCompliance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.DashIsoHbbtvCompliance
  ( DashIsoHbbtvCompliance
      ( DashIsoHbbtvCompliance',
        DIHCHbbtv15,
        DIHCNone
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Supports HbbTV specification as indicated
newtype DashIsoHbbtvCompliance = DashIsoHbbtvCompliance' Lude.Text
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

pattern DIHCHbbtv15 :: DashIsoHbbtvCompliance
pattern DIHCHbbtv15 = DashIsoHbbtvCompliance' "HBBTV_1_5"

pattern DIHCNone :: DashIsoHbbtvCompliance
pattern DIHCNone = DashIsoHbbtvCompliance' "NONE"

{-# COMPLETE
  DIHCHbbtv15,
  DIHCNone,
  DashIsoHbbtvCompliance'
  #-}
