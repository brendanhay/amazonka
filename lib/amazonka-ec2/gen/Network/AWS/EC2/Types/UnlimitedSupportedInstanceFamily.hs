{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.UnlimitedSupportedInstanceFamily
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.UnlimitedSupportedInstanceFamily
  ( UnlimitedSupportedInstanceFamily
      ( UnlimitedSupportedInstanceFamily',
        UnlimitedSupportedInstanceFamilyT2,
        UnlimitedSupportedInstanceFamilyT3,
        UnlimitedSupportedInstanceFamilyT3a,
        UnlimitedSupportedInstanceFamilyT4g,
        fromUnlimitedSupportedInstanceFamily
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype UnlimitedSupportedInstanceFamily = UnlimitedSupportedInstanceFamily'
  { fromUnlimitedSupportedInstanceFamily ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern UnlimitedSupportedInstanceFamilyT2 :: UnlimitedSupportedInstanceFamily
pattern UnlimitedSupportedInstanceFamilyT2 = UnlimitedSupportedInstanceFamily' "t2"

pattern UnlimitedSupportedInstanceFamilyT3 :: UnlimitedSupportedInstanceFamily
pattern UnlimitedSupportedInstanceFamilyT3 = UnlimitedSupportedInstanceFamily' "t3"

pattern UnlimitedSupportedInstanceFamilyT3a :: UnlimitedSupportedInstanceFamily
pattern UnlimitedSupportedInstanceFamilyT3a = UnlimitedSupportedInstanceFamily' "t3a"

pattern UnlimitedSupportedInstanceFamilyT4g :: UnlimitedSupportedInstanceFamily
pattern UnlimitedSupportedInstanceFamilyT4g = UnlimitedSupportedInstanceFamily' "t4g"

{-# COMPLETE
  UnlimitedSupportedInstanceFamilyT2,
  UnlimitedSupportedInstanceFamilyT3,
  UnlimitedSupportedInstanceFamilyT3a,
  UnlimitedSupportedInstanceFamilyT4g,
  UnlimitedSupportedInstanceFamily'
  #-}
