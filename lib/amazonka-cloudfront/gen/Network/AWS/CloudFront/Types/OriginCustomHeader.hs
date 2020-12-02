{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.OriginCustomHeader
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.OriginCustomHeader where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A complex type that contains @HeaderName@ and @HeaderValue@ elements, if any, for this distribution.
--
--
--
-- /See:/ 'originCustomHeader' smart constructor.
data OriginCustomHeader = OriginCustomHeader'
  { _ochHeaderName ::
      !Text,
    _ochHeaderValue :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'OriginCustomHeader' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ochHeaderName' - The name of a header that you want CloudFront to send to your origin. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/forward-custom-headers.html Adding Custom Headers to Origin Requests> in the /Amazon CloudFront Developer Guide/ .
--
-- * 'ochHeaderValue' - The value for the header that you specified in the @HeaderName@ field.
originCustomHeader ::
  -- | 'ochHeaderName'
  Text ->
  -- | 'ochHeaderValue'
  Text ->
  OriginCustomHeader
originCustomHeader pHeaderName_ pHeaderValue_ =
  OriginCustomHeader'
    { _ochHeaderName = pHeaderName_,
      _ochHeaderValue = pHeaderValue_
    }

-- | The name of a header that you want CloudFront to send to your origin. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/forward-custom-headers.html Adding Custom Headers to Origin Requests> in the /Amazon CloudFront Developer Guide/ .
ochHeaderName :: Lens' OriginCustomHeader Text
ochHeaderName = lens _ochHeaderName (\s a -> s {_ochHeaderName = a})

-- | The value for the header that you specified in the @HeaderName@ field.
ochHeaderValue :: Lens' OriginCustomHeader Text
ochHeaderValue = lens _ochHeaderValue (\s a -> s {_ochHeaderValue = a})

instance FromXML OriginCustomHeader where
  parseXML x =
    OriginCustomHeader'
      <$> (x .@ "HeaderName") <*> (x .@ "HeaderValue")

instance Hashable OriginCustomHeader

instance NFData OriginCustomHeader

instance ToXML OriginCustomHeader where
  toXML OriginCustomHeader' {..} =
    mconcat
      ["HeaderName" @= _ochHeaderName, "HeaderValue" @= _ochHeaderValue]
