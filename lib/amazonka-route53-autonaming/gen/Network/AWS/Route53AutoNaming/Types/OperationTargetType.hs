{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53AutoNaming.Types.OperationTargetType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Route53AutoNaming.Types.OperationTargetType
  ( OperationTargetType
    ( OperationTargetType'
    , OperationTargetTypeOTTNamespace
    , OperationTargetTypeOTTService
    , OperationTargetTypeOTTInstance
    , fromOperationTargetType
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype OperationTargetType = OperationTargetType'{fromOperationTargetType
                                                   :: Core.Text}
                                deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                Core.Generic)
                                deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                  Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                  Core.FromJSON, Core.ToXML, Core.FromXML,
                                                  Core.ToText, Core.FromText, Core.ToByteString,
                                                  Core.ToQuery, Core.ToHeader)

pattern OperationTargetTypeOTTNamespace :: OperationTargetType
pattern OperationTargetTypeOTTNamespace = OperationTargetType' "NAMESPACE"

pattern OperationTargetTypeOTTService :: OperationTargetType
pattern OperationTargetTypeOTTService = OperationTargetType' "SERVICE"

pattern OperationTargetTypeOTTInstance :: OperationTargetType
pattern OperationTargetTypeOTTInstance = OperationTargetType' "INSTANCE"

{-# COMPLETE 
  OperationTargetTypeOTTNamespace,

  OperationTargetTypeOTTService,

  OperationTargetTypeOTTInstance,
  OperationTargetType'
  #-}
