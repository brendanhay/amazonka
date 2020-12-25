{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53AutoNaming.Types.OperationType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53AutoNaming.Types.OperationType
  ( OperationType
      ( OperationType',
        OperationTypeCreateNamespace,
        OperationTypeDeleteNamespace,
        OperationTypeUpdateService,
        OperationTypeRegisterInstance,
        OperationTypeDeregisterInstance,
        fromOperationType
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype OperationType = OperationType'
  { fromOperationType ::
      Core.Text
  }
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

pattern OperationTypeCreateNamespace :: OperationType
pattern OperationTypeCreateNamespace = OperationType' "CREATE_NAMESPACE"

pattern OperationTypeDeleteNamespace :: OperationType
pattern OperationTypeDeleteNamespace = OperationType' "DELETE_NAMESPACE"

pattern OperationTypeUpdateService :: OperationType
pattern OperationTypeUpdateService = OperationType' "UPDATE_SERVICE"

pattern OperationTypeRegisterInstance :: OperationType
pattern OperationTypeRegisterInstance = OperationType' "REGISTER_INSTANCE"

pattern OperationTypeDeregisterInstance :: OperationType
pattern OperationTypeDeregisterInstance = OperationType' "DEREGISTER_INSTANCE"

{-# COMPLETE
  OperationTypeCreateNamespace,
  OperationTypeDeleteNamespace,
  OperationTypeUpdateService,
  OperationTypeRegisterInstance,
  OperationTypeDeregisterInstance,
  OperationType'
  #-}
