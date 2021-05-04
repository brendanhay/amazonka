{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.Types.IntegrationType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.APIGateway.Types.IntegrationType
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

import qualified Network.AWS.Prelude as Prelude

-- | The integration type. The valid value is @HTTP@ for integrating an API
-- method with an HTTP backend; @AWS@ with any AWS service endpoints;
-- @MOCK@ for testing without actually invoking the backend; @HTTP_PROXY@
-- for integrating with the HTTP proxy integration; @AWS_PROXY@ for
-- integrating with the Lambda proxy integration.
newtype IntegrationType = IntegrationType'
  { fromIntegrationType ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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
