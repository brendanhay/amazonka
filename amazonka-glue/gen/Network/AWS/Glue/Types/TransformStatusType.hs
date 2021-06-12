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
-- Module      : Network.AWS.Glue.Types.TransformStatusType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.TransformStatusType
  ( TransformStatusType
      ( ..,
        TransformStatusType_DELETING,
        TransformStatusType_NOT_READY,
        TransformStatusType_READY
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype TransformStatusType = TransformStatusType'
  { fromTransformStatusType ::
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

pattern TransformStatusType_DELETING :: TransformStatusType
pattern TransformStatusType_DELETING = TransformStatusType' "DELETING"

pattern TransformStatusType_NOT_READY :: TransformStatusType
pattern TransformStatusType_NOT_READY = TransformStatusType' "NOT_READY"

pattern TransformStatusType_READY :: TransformStatusType
pattern TransformStatusType_READY = TransformStatusType' "READY"

{-# COMPLETE
  TransformStatusType_DELETING,
  TransformStatusType_NOT_READY,
  TransformStatusType_READY,
  TransformStatusType'
  #-}
