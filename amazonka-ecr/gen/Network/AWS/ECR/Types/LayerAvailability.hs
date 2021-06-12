{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECR.Types.LayerAvailability
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECR.Types.LayerAvailability
  ( LayerAvailability
      ( ..,
        LayerAvailability_AVAILABLE,
        LayerAvailability_UNAVAILABLE
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype LayerAvailability = LayerAvailability'
  { fromLayerAvailability ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
    )

pattern LayerAvailability_AVAILABLE :: LayerAvailability
pattern LayerAvailability_AVAILABLE = LayerAvailability' "AVAILABLE"

pattern LayerAvailability_UNAVAILABLE :: LayerAvailability
pattern LayerAvailability_UNAVAILABLE = LayerAvailability' "UNAVAILABLE"

{-# COMPLETE
  LayerAvailability_AVAILABLE,
  LayerAvailability_UNAVAILABLE,
  LayerAvailability'
  #-}
