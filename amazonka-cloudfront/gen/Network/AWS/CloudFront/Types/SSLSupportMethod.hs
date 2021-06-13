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
-- Module      : Network.AWS.CloudFront.Types.SSLSupportMethod
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.SSLSupportMethod
  ( SSLSupportMethod
      ( ..,
        SSLSupportMethod_Sni_only,
        SSLSupportMethod_Static_ip,
        SSLSupportMethod_Vip
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype SSLSupportMethod = SSLSupportMethod'
  { fromSSLSupportMethod ::
      Core.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
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

pattern SSLSupportMethod_Sni_only :: SSLSupportMethod
pattern SSLSupportMethod_Sni_only = SSLSupportMethod' "sni-only"

pattern SSLSupportMethod_Static_ip :: SSLSupportMethod
pattern SSLSupportMethod_Static_ip = SSLSupportMethod' "static-ip"

pattern SSLSupportMethod_Vip :: SSLSupportMethod
pattern SSLSupportMethod_Vip = SSLSupportMethod' "vip"

{-# COMPLETE
  SSLSupportMethod_Sni_only,
  SSLSupportMethod_Static_ip,
  SSLSupportMethod_Vip,
  SSLSupportMethod'
  #-}
