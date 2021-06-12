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
-- Module      : Network.AWS.Route53AutoNaming.Types.OperationTargetType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53AutoNaming.Types.OperationTargetType
  ( OperationTargetType
      ( ..,
        OperationTargetType_INSTANCE,
        OperationTargetType_NAMESPACE,
        OperationTargetType_SERVICE
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype OperationTargetType = OperationTargetType'
  { fromOperationTargetType ::
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

pattern OperationTargetType_INSTANCE :: OperationTargetType
pattern OperationTargetType_INSTANCE = OperationTargetType' "INSTANCE"

pattern OperationTargetType_NAMESPACE :: OperationTargetType
pattern OperationTargetType_NAMESPACE = OperationTargetType' "NAMESPACE"

pattern OperationTargetType_SERVICE :: OperationTargetType
pattern OperationTargetType_SERVICE = OperationTargetType' "SERVICE"

{-# COMPLETE
  OperationTargetType_INSTANCE,
  OperationTargetType_NAMESPACE,
  OperationTargetType_SERVICE,
  OperationTargetType'
  #-}
