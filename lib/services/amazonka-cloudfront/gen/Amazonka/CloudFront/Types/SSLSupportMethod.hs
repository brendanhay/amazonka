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
-- Module      : Amazonka.CloudFront.Types.SSLSupportMethod
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFront.Types.SSLSupportMethod
  ( SSLSupportMethod
      ( ..,
        SSLSupportMethod_Sni_only,
        SSLSupportMethod_Static_ip,
        SSLSupportMethod_Vip
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype SSLSupportMethod = SSLSupportMethod'
  { fromSSLSupportMethod ::
      Data.Text
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
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
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
