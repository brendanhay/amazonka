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
-- Module      : Amazonka.ApiGatewayV2.Types.IntegrationType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ApiGatewayV2.Types.IntegrationType
  ( IntegrationType
      ( ..,
        IntegrationType_AWS,
        IntegrationType_AWS_PROXY,
        IntegrationType_HTTP,
        IntegrationType_HTTP_PROXY,
        IntegrationType_MOCK
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents an API method integration type.
newtype IntegrationType = IntegrationType'
  { fromIntegrationType ::
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

pattern IntegrationType_AWS :: IntegrationType
pattern IntegrationType_AWS = IntegrationType' "AWS"

pattern IntegrationType_AWS_PROXY :: IntegrationType
pattern IntegrationType_AWS_PROXY = IntegrationType' "AWS_PROXY"

pattern IntegrationType_HTTP :: IntegrationType
pattern IntegrationType_HTTP = IntegrationType' "HTTP"

pattern IntegrationType_HTTP_PROXY :: IntegrationType
pattern IntegrationType_HTTP_PROXY = IntegrationType' "HTTP_PROXY"

pattern IntegrationType_MOCK :: IntegrationType
pattern IntegrationType_MOCK = IntegrationType' "MOCK"

{-# COMPLETE
  IntegrationType_AWS,
  IntegrationType_AWS_PROXY,
  IntegrationType_HTTP,
  IntegrationType_HTTP_PROXY,
  IntegrationType_MOCK,
  IntegrationType'
  #-}
