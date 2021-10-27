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
-- Module      : Network.AWS.RAM.Types.ResourceShareFeatureSet
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RAM.Types.ResourceShareFeatureSet
  ( ResourceShareFeatureSet
      ( ..,
        ResourceShareFeatureSet_CREATED_FROM_POLICY,
        ResourceShareFeatureSet_PROMOTING_TO_STANDARD,
        ResourceShareFeatureSet_STANDARD
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype ResourceShareFeatureSet = ResourceShareFeatureSet'
  { fromResourceShareFeatureSet ::
      Core.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
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

pattern ResourceShareFeatureSet_CREATED_FROM_POLICY :: ResourceShareFeatureSet
pattern ResourceShareFeatureSet_CREATED_FROM_POLICY = ResourceShareFeatureSet' "CREATED_FROM_POLICY"

pattern ResourceShareFeatureSet_PROMOTING_TO_STANDARD :: ResourceShareFeatureSet
pattern ResourceShareFeatureSet_PROMOTING_TO_STANDARD = ResourceShareFeatureSet' "PROMOTING_TO_STANDARD"

pattern ResourceShareFeatureSet_STANDARD :: ResourceShareFeatureSet
pattern ResourceShareFeatureSet_STANDARD = ResourceShareFeatureSet' "STANDARD"

{-# COMPLETE
  ResourceShareFeatureSet_CREATED_FROM_POLICY,
  ResourceShareFeatureSet_PROMOTING_TO_STANDARD,
  ResourceShareFeatureSet_STANDARD,
  ResourceShareFeatureSet'
  #-}
