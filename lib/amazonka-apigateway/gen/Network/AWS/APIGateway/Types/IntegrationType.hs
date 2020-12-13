{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.Types.IntegrationType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.APIGateway.Types.IntegrationType
  ( IntegrationType
      ( IntegrationType',
        HTTP,
        AWS,
        Mock,
        HTTPProxy,
        AWSProxy
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | The integration type. The valid value is @HTTP@ for integrating an API method with an HTTP backend; @AWS@ with any AWS service endpoints; @MOCK@ for testing without actually invoking the backend; @HTTP_PROXY@ for integrating with the HTTP proxy integration; @AWS_PROXY@ for integrating with the Lambda proxy integration.
newtype IntegrationType = IntegrationType' Lude.Text
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

pattern HTTP :: IntegrationType
pattern HTTP = IntegrationType' "HTTP"

pattern AWS :: IntegrationType
pattern AWS = IntegrationType' "AWS"

pattern Mock :: IntegrationType
pattern Mock = IntegrationType' "MOCK"

pattern HTTPProxy :: IntegrationType
pattern HTTPProxy = IntegrationType' "HTTP_PROXY"

pattern AWSProxy :: IntegrationType
pattern AWSProxy = IntegrationType' "AWS_PROXY"

{-# COMPLETE
  HTTP,
  AWS,
  Mock,
  HTTPProxy,
  AWSProxy,
  IntegrationType'
  #-}
