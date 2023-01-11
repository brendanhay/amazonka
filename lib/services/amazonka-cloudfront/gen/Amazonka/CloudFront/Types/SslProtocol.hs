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
-- Module      : Amazonka.CloudFront.Types.SslProtocol
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFront.Types.SslProtocol
  ( SslProtocol
      ( ..,
        SslProtocol_SSLv3,
        SslProtocol_TLSv1,
        SslProtocol_TLSv1_1,
        SslProtocol_TLSv1_2
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype SslProtocol = SslProtocol'
  { fromSslProtocol ::
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

pattern SslProtocol_SSLv3 :: SslProtocol
pattern SslProtocol_SSLv3 = SslProtocol' "SSLv3"

pattern SslProtocol_TLSv1 :: SslProtocol
pattern SslProtocol_TLSv1 = SslProtocol' "TLSv1"

pattern SslProtocol_TLSv1_1 :: SslProtocol
pattern SslProtocol_TLSv1_1 = SslProtocol' "TLSv1.1"

pattern SslProtocol_TLSv1_2 :: SslProtocol
pattern SslProtocol_TLSv1_2 = SslProtocol' "TLSv1.2"

{-# COMPLETE
  SslProtocol_SSLv3,
  SslProtocol_TLSv1,
  SslProtocol_TLSv1_1,
  SslProtocol_TLSv1_2,
  SslProtocol'
  #-}
