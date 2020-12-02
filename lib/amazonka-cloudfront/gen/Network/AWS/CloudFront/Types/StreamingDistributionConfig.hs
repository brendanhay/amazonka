{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.StreamingDistributionConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.StreamingDistributionConfig where

import Network.AWS.CloudFront.Types.Aliases
import Network.AWS.CloudFront.Types.PriceClass
import Network.AWS.CloudFront.Types.S3Origin
import Network.AWS.CloudFront.Types.StreamingLoggingConfig
import Network.AWS.CloudFront.Types.TrustedSigners
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The RTMP distribution's configuration information.
--
--
--
-- /See:/ 'streamingDistributionConfig' smart constructor.
data StreamingDistributionConfig = StreamingDistributionConfig'
  { _sdcAliases ::
      !(Maybe Aliases),
    _sdcPriceClass ::
      !(Maybe PriceClass),
    _sdcLogging ::
      !(Maybe StreamingLoggingConfig),
    _sdcCallerReference :: !Text,
    _sdcS3Origin :: !S3Origin,
    _sdcComment :: !Text,
    _sdcTrustedSigners ::
      !TrustedSigners,
    _sdcEnabled :: !Bool
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StreamingDistributionConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sdcAliases' - A complex type that contains information about CNAMEs (alternate domain names), if any, for this streaming distribution.
--
-- * 'sdcPriceClass' - A complex type that contains information about price class for this streaming distribution.
--
-- * 'sdcLogging' - A complex type that controls whether access logs are written for the streaming distribution.
--
-- * 'sdcCallerReference' - A unique value (for example, a date-time stamp) that ensures that the request can't be replayed. If the value of @CallerReference@ is new (regardless of the content of the @StreamingDistributionConfig@ object), CloudFront creates a new distribution. If @CallerReference@ is a value that you already sent in a previous request to create a distribution, CloudFront returns a @DistributionAlreadyExists@ error.
--
-- * 'sdcS3Origin' - A complex type that contains information about the Amazon S3 bucket from which you want CloudFront to get your media files for distribution.
--
-- * 'sdcComment' - Any comments you want to include about the streaming distribution.
--
-- * 'sdcTrustedSigners' - A complex type that specifies any AWS accounts that you want to permit to create signed URLs for private content. If you want the distribution to use signed URLs, include this element; if you want the distribution to use public URLs, remove this element. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PrivateContent.html Serving Private Content through CloudFront> in the /Amazon CloudFront Developer Guide/ .
--
-- * 'sdcEnabled' - Whether the streaming distribution is enabled to accept user requests for content.
streamingDistributionConfig ::
  -- | 'sdcCallerReference'
  Text ->
  -- | 'sdcS3Origin'
  S3Origin ->
  -- | 'sdcComment'
  Text ->
  -- | 'sdcTrustedSigners'
  TrustedSigners ->
  -- | 'sdcEnabled'
  Bool ->
  StreamingDistributionConfig
streamingDistributionConfig
  pCallerReference_
  pS3Origin_
  pComment_
  pTrustedSigners_
  pEnabled_ =
    StreamingDistributionConfig'
      { _sdcAliases = Nothing,
        _sdcPriceClass = Nothing,
        _sdcLogging = Nothing,
        _sdcCallerReference = pCallerReference_,
        _sdcS3Origin = pS3Origin_,
        _sdcComment = pComment_,
        _sdcTrustedSigners = pTrustedSigners_,
        _sdcEnabled = pEnabled_
      }

-- | A complex type that contains information about CNAMEs (alternate domain names), if any, for this streaming distribution.
sdcAliases :: Lens' StreamingDistributionConfig (Maybe Aliases)
sdcAliases = lens _sdcAliases (\s a -> s {_sdcAliases = a})

-- | A complex type that contains information about price class for this streaming distribution.
sdcPriceClass :: Lens' StreamingDistributionConfig (Maybe PriceClass)
sdcPriceClass = lens _sdcPriceClass (\s a -> s {_sdcPriceClass = a})

-- | A complex type that controls whether access logs are written for the streaming distribution.
sdcLogging :: Lens' StreamingDistributionConfig (Maybe StreamingLoggingConfig)
sdcLogging = lens _sdcLogging (\s a -> s {_sdcLogging = a})

-- | A unique value (for example, a date-time stamp) that ensures that the request can't be replayed. If the value of @CallerReference@ is new (regardless of the content of the @StreamingDistributionConfig@ object), CloudFront creates a new distribution. If @CallerReference@ is a value that you already sent in a previous request to create a distribution, CloudFront returns a @DistributionAlreadyExists@ error.
sdcCallerReference :: Lens' StreamingDistributionConfig Text
sdcCallerReference = lens _sdcCallerReference (\s a -> s {_sdcCallerReference = a})

-- | A complex type that contains information about the Amazon S3 bucket from which you want CloudFront to get your media files for distribution.
sdcS3Origin :: Lens' StreamingDistributionConfig S3Origin
sdcS3Origin = lens _sdcS3Origin (\s a -> s {_sdcS3Origin = a})

-- | Any comments you want to include about the streaming distribution.
sdcComment :: Lens' StreamingDistributionConfig Text
sdcComment = lens _sdcComment (\s a -> s {_sdcComment = a})

-- | A complex type that specifies any AWS accounts that you want to permit to create signed URLs for private content. If you want the distribution to use signed URLs, include this element; if you want the distribution to use public URLs, remove this element. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PrivateContent.html Serving Private Content through CloudFront> in the /Amazon CloudFront Developer Guide/ .
sdcTrustedSigners :: Lens' StreamingDistributionConfig TrustedSigners
sdcTrustedSigners = lens _sdcTrustedSigners (\s a -> s {_sdcTrustedSigners = a})

-- | Whether the streaming distribution is enabled to accept user requests for content.
sdcEnabled :: Lens' StreamingDistributionConfig Bool
sdcEnabled = lens _sdcEnabled (\s a -> s {_sdcEnabled = a})

instance FromXML StreamingDistributionConfig where
  parseXML x =
    StreamingDistributionConfig'
      <$> (x .@? "Aliases")
      <*> (x .@? "PriceClass")
      <*> (x .@? "Logging")
      <*> (x .@ "CallerReference")
      <*> (x .@ "S3Origin")
      <*> (x .@ "Comment")
      <*> (x .@ "TrustedSigners")
      <*> (x .@ "Enabled")

instance Hashable StreamingDistributionConfig

instance NFData StreamingDistributionConfig

instance ToXML StreamingDistributionConfig where
  toXML StreamingDistributionConfig' {..} =
    mconcat
      [ "Aliases" @= _sdcAliases,
        "PriceClass" @= _sdcPriceClass,
        "Logging" @= _sdcLogging,
        "CallerReference" @= _sdcCallerReference,
        "S3Origin" @= _sdcS3Origin,
        "Comment" @= _sdcComment,
        "TrustedSigners" @= _sdcTrustedSigners,
        "Enabled" @= _sdcEnabled
      ]
