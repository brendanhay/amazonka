{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.Types.Compute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkSpaces.Types.Compute
  ( Compute
      ( Compute',
        ComputeValue,
        ComputeStandard,
        ComputePerformance,
        ComputePower,
        ComputeGraphics,
        ComputePowerpro,
        ComputeGraphicspro,
        fromCompute
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype Compute = Compute' {fromCompute :: Core.Text}
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

pattern ComputeValue :: Compute
pattern ComputeValue = Compute' "VALUE"

pattern ComputeStandard :: Compute
pattern ComputeStandard = Compute' "STANDARD"

pattern ComputePerformance :: Compute
pattern ComputePerformance = Compute' "PERFORMANCE"

pattern ComputePower :: Compute
pattern ComputePower = Compute' "POWER"

pattern ComputeGraphics :: Compute
pattern ComputeGraphics = Compute' "GRAPHICS"

pattern ComputePowerpro :: Compute
pattern ComputePowerpro = Compute' "POWERPRO"

pattern ComputeGraphicspro :: Compute
pattern ComputeGraphicspro = Compute' "GRAPHICSPRO"

{-# COMPLETE
  ComputeValue,
  ComputeStandard,
  ComputePerformance,
  ComputePower,
  ComputeGraphics,
  ComputePowerpro,
  ComputeGraphicspro,
  Compute'
  #-}
