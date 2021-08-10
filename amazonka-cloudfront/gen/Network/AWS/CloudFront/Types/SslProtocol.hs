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
-- Module      : Network.AWS.CloudFront.Types.SslProtocol
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.SslProtocol
  ( SslProtocol
      ( ..,
        SslProtocol_SSLv3,
        SslProtocol_TLSv1,
        SslProtocol_TLSv1_1,
        SslProtocol_TLSv1_2
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype SslProtocol = SslProtocol'
  { fromSslProtocol ::
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
