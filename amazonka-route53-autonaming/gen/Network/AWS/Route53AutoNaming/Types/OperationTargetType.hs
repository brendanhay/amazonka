{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
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

import qualified Network.AWS.Prelude as Prelude

newtype OperationTargetType = OperationTargetType'
  { fromOperationTargetType ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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
