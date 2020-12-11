-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.Types.SecurityPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.APIGateway.Types.SecurityPolicy
  ( SecurityPolicy
      ( SecurityPolicy',
        TLS10,
        TLS12
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype SecurityPolicy = SecurityPolicy' Lude.Text
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

pattern TLS10 :: SecurityPolicy
pattern TLS10 = SecurityPolicy' "TLS_1_0"

pattern TLS12 :: SecurityPolicy
pattern TLS12 = SecurityPolicy' "TLS_1_2"

{-# COMPLETE
  TLS10,
  TLS12,
  SecurityPolicy'
  #-}
