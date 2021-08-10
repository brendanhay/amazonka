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
-- Module      : Network.AWS.Route53AutoNaming.Types.OperationType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53AutoNaming.Types.OperationType
  ( OperationType
      ( ..,
        OperationType_CREATE_NAMESPACE,
        OperationType_DELETE_NAMESPACE,
        OperationType_DEREGISTER_INSTANCE,
        OperationType_REGISTER_INSTANCE,
        OperationType_UPDATE_SERVICE
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype OperationType = OperationType'
  { fromOperationType ::
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

pattern OperationType_CREATE_NAMESPACE :: OperationType
pattern OperationType_CREATE_NAMESPACE = OperationType' "CREATE_NAMESPACE"

pattern OperationType_DELETE_NAMESPACE :: OperationType
pattern OperationType_DELETE_NAMESPACE = OperationType' "DELETE_NAMESPACE"

pattern OperationType_DEREGISTER_INSTANCE :: OperationType
pattern OperationType_DEREGISTER_INSTANCE = OperationType' "DEREGISTER_INSTANCE"

pattern OperationType_REGISTER_INSTANCE :: OperationType
pattern OperationType_REGISTER_INSTANCE = OperationType' "REGISTER_INSTANCE"

pattern OperationType_UPDATE_SERVICE :: OperationType
pattern OperationType_UPDATE_SERVICE = OperationType' "UPDATE_SERVICE"

{-# COMPLETE
  OperationType_CREATE_NAMESPACE,
  OperationType_DELETE_NAMESPACE,
  OperationType_DEREGISTER_INSTANCE,
  OperationType_REGISTER_INSTANCE,
  OperationType_UPDATE_SERVICE,
  OperationType'
  #-}
