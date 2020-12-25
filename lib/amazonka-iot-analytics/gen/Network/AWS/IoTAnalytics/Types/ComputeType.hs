{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.ComputeType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.ComputeType
  ( ComputeType
      ( ComputeType',
        ComputeTypeAcu1,
        ComputeTypeAcu2,
        fromComputeType
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype ComputeType = ComputeType' {fromComputeType :: Core.Text}
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

pattern ComputeTypeAcu1 :: ComputeType
pattern ComputeTypeAcu1 = ComputeType' "ACU_1"

pattern ComputeTypeAcu2 :: ComputeType
pattern ComputeTypeAcu2 = ComputeType' "ACU_2"

{-# COMPLETE
  ComputeTypeAcu1,
  ComputeTypeAcu2,
  ComputeType'
  #-}
