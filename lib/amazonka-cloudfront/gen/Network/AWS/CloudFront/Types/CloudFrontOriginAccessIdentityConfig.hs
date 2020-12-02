{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.CloudFrontOriginAccessIdentityConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.CloudFrontOriginAccessIdentityConfig where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Origin access identity configuration. Send a @GET@ request to the @//CloudFront API version/ /CloudFront/identity ID/config@ resource.
--
--
--
-- /See:/ 'cloudFrontOriginAccessIdentityConfig' smart constructor.
data CloudFrontOriginAccessIdentityConfig = CloudFrontOriginAccessIdentityConfig'
  { _cfoaicCallerReference ::
      !Text,
    _cfoaicComment ::
      !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CloudFrontOriginAccessIdentityConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cfoaicCallerReference' - A unique value (for example, a date-time stamp) that ensures that the request can't be replayed. If the value of @CallerReference@ is new (regardless of the content of the @CloudFrontOriginAccessIdentityConfig@ object), a new origin access identity is created. If the @CallerReference@ is a value already sent in a previous identity request, and the content of the @CloudFrontOriginAccessIdentityConfig@ is identical to the original request (ignoring white space), the response includes the same information returned to the original request.  If the @CallerReference@ is a value you already sent in a previous request to create an identity, but the content of the @CloudFrontOriginAccessIdentityConfig@ is different from the original request, CloudFront returns a @CloudFrontOriginAccessIdentityAlreadyExists@ error.
--
-- * 'cfoaicComment' - Any comments you want to include about the origin access identity.
cloudFrontOriginAccessIdentityConfig ::
  -- | 'cfoaicCallerReference'
  Text ->
  -- | 'cfoaicComment'
  Text ->
  CloudFrontOriginAccessIdentityConfig
cloudFrontOriginAccessIdentityConfig pCallerReference_ pComment_ =
  CloudFrontOriginAccessIdentityConfig'
    { _cfoaicCallerReference =
        pCallerReference_,
      _cfoaicComment = pComment_
    }

-- | A unique value (for example, a date-time stamp) that ensures that the request can't be replayed. If the value of @CallerReference@ is new (regardless of the content of the @CloudFrontOriginAccessIdentityConfig@ object), a new origin access identity is created. If the @CallerReference@ is a value already sent in a previous identity request, and the content of the @CloudFrontOriginAccessIdentityConfig@ is identical to the original request (ignoring white space), the response includes the same information returned to the original request.  If the @CallerReference@ is a value you already sent in a previous request to create an identity, but the content of the @CloudFrontOriginAccessIdentityConfig@ is different from the original request, CloudFront returns a @CloudFrontOriginAccessIdentityAlreadyExists@ error.
cfoaicCallerReference :: Lens' CloudFrontOriginAccessIdentityConfig Text
cfoaicCallerReference = lens _cfoaicCallerReference (\s a -> s {_cfoaicCallerReference = a})

-- | Any comments you want to include about the origin access identity.
cfoaicComment :: Lens' CloudFrontOriginAccessIdentityConfig Text
cfoaicComment = lens _cfoaicComment (\s a -> s {_cfoaicComment = a})

instance FromXML CloudFrontOriginAccessIdentityConfig where
  parseXML x =
    CloudFrontOriginAccessIdentityConfig'
      <$> (x .@ "CallerReference") <*> (x .@ "Comment")

instance Hashable CloudFrontOriginAccessIdentityConfig

instance NFData CloudFrontOriginAccessIdentityConfig

instance ToXML CloudFrontOriginAccessIdentityConfig where
  toXML CloudFrontOriginAccessIdentityConfig' {..} =
    mconcat
      [ "CallerReference" @= _cfoaicCallerReference,
        "Comment" @= _cfoaicComment
      ]
