{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.StreamingLoggingConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.StreamingLoggingConfig where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A complex type that controls whether access logs are written for this streaming distribution.
--
--
--
-- /See:/ 'streamingLoggingConfig' smart constructor.
data StreamingLoggingConfig = StreamingLoggingConfig'
  { _slcEnabled ::
      !Bool,
    _slcBucket :: !Text,
    _slcPrefix :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StreamingLoggingConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'slcEnabled' - Specifies whether you want CloudFront to save access logs to an Amazon S3 bucket. If you don't want to enable logging when you create a streaming distribution or if you want to disable logging for an existing streaming distribution, specify @false@ for @Enabled@ , and specify @empty Bucket@ and @Prefix@ elements. If you specify @false@ for @Enabled@ but you specify values for @Bucket@ and @Prefix@ , the values are automatically deleted.
--
-- * 'slcBucket' - The Amazon S3 bucket to store the access logs in, for example, @myawslogbucket.s3.amazonaws.com@ .
--
-- * 'slcPrefix' - An optional string that you want CloudFront to prefix to the access log filenames for this streaming distribution, for example, @myprefix/@ . If you want to enable logging, but you don't want to specify a prefix, you still must include an empty @Prefix@ element in the @Logging@ element.
streamingLoggingConfig ::
  -- | 'slcEnabled'
  Bool ->
  -- | 'slcBucket'
  Text ->
  -- | 'slcPrefix'
  Text ->
  StreamingLoggingConfig
streamingLoggingConfig pEnabled_ pBucket_ pPrefix_ =
  StreamingLoggingConfig'
    { _slcEnabled = pEnabled_,
      _slcBucket = pBucket_,
      _slcPrefix = pPrefix_
    }

-- | Specifies whether you want CloudFront to save access logs to an Amazon S3 bucket. If you don't want to enable logging when you create a streaming distribution or if you want to disable logging for an existing streaming distribution, specify @false@ for @Enabled@ , and specify @empty Bucket@ and @Prefix@ elements. If you specify @false@ for @Enabled@ but you specify values for @Bucket@ and @Prefix@ , the values are automatically deleted.
slcEnabled :: Lens' StreamingLoggingConfig Bool
slcEnabled = lens _slcEnabled (\s a -> s {_slcEnabled = a})

-- | The Amazon S3 bucket to store the access logs in, for example, @myawslogbucket.s3.amazonaws.com@ .
slcBucket :: Lens' StreamingLoggingConfig Text
slcBucket = lens _slcBucket (\s a -> s {_slcBucket = a})

-- | An optional string that you want CloudFront to prefix to the access log filenames for this streaming distribution, for example, @myprefix/@ . If you want to enable logging, but you don't want to specify a prefix, you still must include an empty @Prefix@ element in the @Logging@ element.
slcPrefix :: Lens' StreamingLoggingConfig Text
slcPrefix = lens _slcPrefix (\s a -> s {_slcPrefix = a})

instance FromXML StreamingLoggingConfig where
  parseXML x =
    StreamingLoggingConfig'
      <$> (x .@ "Enabled") <*> (x .@ "Bucket") <*> (x .@ "Prefix")

instance Hashable StreamingLoggingConfig

instance NFData StreamingLoggingConfig

instance ToXML StreamingLoggingConfig where
  toXML StreamingLoggingConfig' {..} =
    mconcat
      [ "Enabled" @= _slcEnabled,
        "Bucket" @= _slcBucket,
        "Prefix" @= _slcPrefix
      ]
