{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.Types.SecurityPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ApiGateway.Types.SecurityPolicy
  ( SecurityPolicy
    ( SecurityPolicy'
    , SecurityPolicyTls10
    , SecurityPolicyTls12
    , fromSecurityPolicy
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype SecurityPolicy = SecurityPolicy'{fromSecurityPolicy ::
                                         Core.Text}
                           deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                           Core.Generic)
                           deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                             Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                             Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                             Core.FromText, Core.ToByteString, Core.ToQuery,
                                             Core.ToHeader)

pattern SecurityPolicyTls10 :: SecurityPolicy
pattern SecurityPolicyTls10 = SecurityPolicy' "TLS_1_0"

pattern SecurityPolicyTls12 :: SecurityPolicy
pattern SecurityPolicyTls12 = SecurityPolicy' "TLS_1_2"

{-# COMPLETE 
  SecurityPolicyTls10,

  SecurityPolicyTls12,
  SecurityPolicy'
  #-}
