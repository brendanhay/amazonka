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
-- Module      : Amazonka.Route53AutoNaming.Types.OperationType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53AutoNaming.Types.OperationType
  ( OperationType
      ( ..,
        OperationType_CREATE_NAMESPACE,
        OperationType_DELETE_NAMESPACE,
        OperationType_DEREGISTER_INSTANCE,
        OperationType_REGISTER_INSTANCE,
        OperationType_UPDATE_NAMESPACE,
        OperationType_UPDATE_SERVICE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype OperationType = OperationType'
  { fromOperationType ::
      Data.Text
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
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
    )

pattern OperationType_CREATE_NAMESPACE :: OperationType
pattern OperationType_CREATE_NAMESPACE = OperationType' "CREATE_NAMESPACE"

pattern OperationType_DELETE_NAMESPACE :: OperationType
pattern OperationType_DELETE_NAMESPACE = OperationType' "DELETE_NAMESPACE"

pattern OperationType_DEREGISTER_INSTANCE :: OperationType
pattern OperationType_DEREGISTER_INSTANCE = OperationType' "DEREGISTER_INSTANCE"

pattern OperationType_REGISTER_INSTANCE :: OperationType
pattern OperationType_REGISTER_INSTANCE = OperationType' "REGISTER_INSTANCE"

pattern OperationType_UPDATE_NAMESPACE :: OperationType
pattern OperationType_UPDATE_NAMESPACE = OperationType' "UPDATE_NAMESPACE"

pattern OperationType_UPDATE_SERVICE :: OperationType
pattern OperationType_UPDATE_SERVICE = OperationType' "UPDATE_SERVICE"

{-# COMPLETE
  OperationType_CREATE_NAMESPACE,
  OperationType_DELETE_NAMESPACE,
  OperationType_DEREGISTER_INSTANCE,
  OperationType_REGISTER_INSTANCE,
  OperationType_UPDATE_NAMESPACE,
  OperationType_UPDATE_SERVICE,
  OperationType'
  #-}
