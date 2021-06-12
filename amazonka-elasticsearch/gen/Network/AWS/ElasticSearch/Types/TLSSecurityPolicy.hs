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
-- Module      : Network.AWS.ElasticSearch.Types.TLSSecurityPolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.TLSSecurityPolicy
  ( TLSSecurityPolicy
      ( ..,
        TLSSecurityPolicy_Policy_Min_TLS_1_0_2019_07,
        TLSSecurityPolicy_Policy_Min_TLS_1_2_2019_07
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype TLSSecurityPolicy = TLSSecurityPolicy'
  { fromTLSSecurityPolicy ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
    )

pattern TLSSecurityPolicy_Policy_Min_TLS_1_0_2019_07 :: TLSSecurityPolicy
pattern TLSSecurityPolicy_Policy_Min_TLS_1_0_2019_07 = TLSSecurityPolicy' "Policy-Min-TLS-1-0-2019-07"

pattern TLSSecurityPolicy_Policy_Min_TLS_1_2_2019_07 :: TLSSecurityPolicy
pattern TLSSecurityPolicy_Policy_Min_TLS_1_2_2019_07 = TLSSecurityPolicy' "Policy-Min-TLS-1-2-2019-07"

{-# COMPLETE
  TLSSecurityPolicy_Policy_Min_TLS_1_0_2019_07,
  TLSSecurityPolicy_Policy_Min_TLS_1_2_2019_07,
  TLSSecurityPolicy'
  #-}
