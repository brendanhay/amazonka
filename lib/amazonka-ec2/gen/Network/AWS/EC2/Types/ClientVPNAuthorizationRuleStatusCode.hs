{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ClientVPNAuthorizationRuleStatusCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ClientVPNAuthorizationRuleStatusCode
  ( ClientVPNAuthorizationRuleStatusCode
      ( ClientVPNAuthorizationRuleStatusCode',
        CVARSCAuthorizing,
        CVARSCActive,
        CVARSCFailed,
        CVARSCRevoking
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ClientVPNAuthorizationRuleStatusCode = ClientVPNAuthorizationRuleStatusCode' Lude.Text
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

pattern CVARSCAuthorizing :: ClientVPNAuthorizationRuleStatusCode
pattern CVARSCAuthorizing = ClientVPNAuthorizationRuleStatusCode' "authorizing"

pattern CVARSCActive :: ClientVPNAuthorizationRuleStatusCode
pattern CVARSCActive = ClientVPNAuthorizationRuleStatusCode' "active"

pattern CVARSCFailed :: ClientVPNAuthorizationRuleStatusCode
pattern CVARSCFailed = ClientVPNAuthorizationRuleStatusCode' "failed"

pattern CVARSCRevoking :: ClientVPNAuthorizationRuleStatusCode
pattern CVARSCRevoking = ClientVPNAuthorizationRuleStatusCode' "revoking"

{-# COMPLETE
  CVARSCAuthorizing,
  CVARSCActive,
  CVARSCFailed,
  CVARSCRevoking,
  ClientVPNAuthorizationRuleStatusCode'
  #-}
