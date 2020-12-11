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
        CreateNamespace,
        DeleteNamespace,
        DeregisterInstance,
        RegisterInstance,
        UpdateService
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype OperationType = OperationType' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern CreateNamespace :: OperationType
pattern CreateNamespace = OperationType' "CREATE_NAMESPACE"

pattern DeleteNamespace :: OperationType
pattern DeleteNamespace = OperationType' "DELETE_NAMESPACE"

pattern DeregisterInstance :: OperationType
pattern DeregisterInstance = OperationType' "DEREGISTER_INSTANCE"

pattern RegisterInstance :: OperationType
pattern RegisterInstance = OperationType' "REGISTER_INSTANCE"

pattern UpdateService :: OperationType
pattern UpdateService = OperationType' "UPDATE_SERVICE"

{-# COMPLETE
  CreateNamespace,
  DeleteNamespace,
  DeregisterInstance,
  RegisterInstance,
  UpdateService,
  OperationType'
  #-}
