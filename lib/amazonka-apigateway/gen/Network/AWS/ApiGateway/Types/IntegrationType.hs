{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.Types.IntegrationType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ApiGateway.Types.IntegrationType
  ( IntegrationType
    ( IntegrationType'
    , IntegrationTypeHttp
    , IntegrationTypeAws
    , IntegrationTypeMock
    , IntegrationTypeHttpProxy
    , IntegrationTypeAwsProxy
    , fromIntegrationType
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | The integration type. The valid value is @HTTP@ for integrating an API method with an HTTP backend; @AWS@ with any AWS service endpoints; @MOCK@ for testing without actually invoking the backend; @HTTP_PROXY@ for integrating with the HTTP proxy integration; @AWS_PROXY@ for integrating with the Lambda proxy integration. 
newtype IntegrationType = IntegrationType'{fromIntegrationType ::
                                           Core.Text}
                            deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                            Core.Generic)
                            deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                              Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                              Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                              Core.FromText, Core.ToByteString, Core.ToQuery,
                                              Core.ToHeader)

pattern IntegrationTypeHttp :: IntegrationType
pattern IntegrationTypeHttp = IntegrationType' "HTTP"

pattern IntegrationTypeAws :: IntegrationType
pattern IntegrationTypeAws = IntegrationType' "AWS"

pattern IntegrationTypeMock :: IntegrationType
pattern IntegrationTypeMock = IntegrationType' "MOCK"

pattern IntegrationTypeHttpProxy :: IntegrationType
pattern IntegrationTypeHttpProxy = IntegrationType' "HTTP_PROXY"

pattern IntegrationTypeAwsProxy :: IntegrationType
pattern IntegrationTypeAwsProxy = IntegrationType' "AWS_PROXY"

{-# COMPLETE 
  IntegrationTypeHttp,

  IntegrationTypeAws,

  IntegrationTypeMock,

  IntegrationTypeHttpProxy,

  IntegrationTypeAwsProxy,
  IntegrationType'
  #-}
