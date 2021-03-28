{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53AutoNaming.Types.OperationFilterName
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Route53AutoNaming.Types.OperationFilterName
  ( OperationFilterName
    ( OperationFilterName'
    , OperationFilterNameNamespaceId
    , OperationFilterNameServiceId
    , OperationFilterNameStatus
    , OperationFilterNameType
    , OperationFilterNameUpdateDate
    , fromOperationFilterName
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype OperationFilterName = OperationFilterName'{fromOperationFilterName
                                                   :: Core.Text}
                                deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                Core.Generic)
                                deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                  Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                  Core.FromJSON, Core.ToXML, Core.FromXML,
                                                  Core.ToText, Core.FromText, Core.ToByteString,
                                                  Core.ToQuery, Core.ToHeader)

pattern OperationFilterNameNamespaceId :: OperationFilterName
pattern OperationFilterNameNamespaceId = OperationFilterName' "NAMESPACE_ID"

pattern OperationFilterNameServiceId :: OperationFilterName
pattern OperationFilterNameServiceId = OperationFilterName' "SERVICE_ID"

pattern OperationFilterNameStatus :: OperationFilterName
pattern OperationFilterNameStatus = OperationFilterName' "STATUS"

pattern OperationFilterNameType :: OperationFilterName
pattern OperationFilterNameType = OperationFilterName' "TYPE"

pattern OperationFilterNameUpdateDate :: OperationFilterName
pattern OperationFilterNameUpdateDate = OperationFilterName' "UPDATE_DATE"

{-# COMPLETE 
  OperationFilterNameNamespaceId,

  OperationFilterNameServiceId,

  OperationFilterNameStatus,

  OperationFilterNameType,

  OperationFilterNameUpdateDate,
  OperationFilterName'
  #-}
