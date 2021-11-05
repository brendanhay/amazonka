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
-- Module      : Network.AWS.ECRPublic.Types.LayerFailureCode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECRPublic.Types.LayerFailureCode
  ( LayerFailureCode
      ( ..,
        LayerFailureCode_InvalidLayerDigest,
        LayerFailureCode_MissingLayerDigest
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype LayerFailureCode = LayerFailureCode'
  { fromLayerFailureCode ::
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

pattern LayerFailureCode_InvalidLayerDigest :: LayerFailureCode
pattern LayerFailureCode_InvalidLayerDigest = LayerFailureCode' "InvalidLayerDigest"

pattern LayerFailureCode_MissingLayerDigest :: LayerFailureCode
pattern LayerFailureCode_MissingLayerDigest = LayerFailureCode' "MissingLayerDigest"

{-# COMPLETE
  LayerFailureCode_InvalidLayerDigest,
  LayerFailureCode_MissingLayerDigest,
  LayerFailureCode'
  #-}
