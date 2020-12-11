-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.TLSSecurityPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.TLSSecurityPolicy
  ( TLSSecurityPolicy
      ( TLSSecurityPolicy',
        PolicyMinTLS10201907,
        PolicyMinTLS12201907
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype TLSSecurityPolicy = TLSSecurityPolicy' Lude.Text
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

pattern PolicyMinTLS10201907 :: TLSSecurityPolicy
pattern PolicyMinTLS10201907 = TLSSecurityPolicy' "Policy-Min-TLS-1-0-2019-07"

pattern PolicyMinTLS12201907 :: TLSSecurityPolicy
pattern PolicyMinTLS12201907 = TLSSecurityPolicy' "Policy-Min-TLS-1-2-2019-07"

{-# COMPLETE
  PolicyMinTLS10201907,
  PolicyMinTLS12201907,
  TLSSecurityPolicy'
  #-}
