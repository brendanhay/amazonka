{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.DomainType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types.DomainType
  ( DomainType
    ( DomainType'
    , DomainTypeEndpoint
    , DomainTypeAwsManaged
    , DomainTypeCustomerManaged
    , fromDomainType
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype DomainType = DomainType'{fromDomainType :: Core.Text}
                       deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                       Core.Generic)
                       deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                         Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                         Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                         Core.FromText, Core.ToByteString, Core.ToQuery,
                                         Core.ToHeader)

pattern DomainTypeEndpoint :: DomainType
pattern DomainTypeEndpoint = DomainType' "ENDPOINT"

pattern DomainTypeAwsManaged :: DomainType
pattern DomainTypeAwsManaged = DomainType' "AWS_MANAGED"

pattern DomainTypeCustomerManaged :: DomainType
pattern DomainTypeCustomerManaged = DomainType' "CUSTOMER_MANAGED"

{-# COMPLETE 
  DomainTypeEndpoint,

  DomainTypeAwsManaged,

  DomainTypeCustomerManaged,
  DomainType'
  #-}
