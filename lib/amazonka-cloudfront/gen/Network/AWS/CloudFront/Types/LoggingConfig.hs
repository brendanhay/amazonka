{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.LoggingConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.LoggingConfig where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A complex type that controls whether access logs are written for the distribution.
--
--
--
-- /See:/ 'loggingConfig' smart constructor.
data LoggingConfig = LoggingConfig'
  { _lcEnabled :: !Bool,
    _lcIncludeCookies :: !Bool,
    _lcBucket :: !Text,
    _lcPrefix :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'LoggingConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lcEnabled' - Specifies whether you want CloudFront to save access logs to an Amazon S3 bucket. If you don't want to enable logging when you create a distribution or if you want to disable logging for an existing distribution, specify @false@ for @Enabled@ , and specify empty @Bucket@ and @Prefix@ elements. If you specify @false@ for @Enabled@ but you specify values for @Bucket@ , @prefix@ , and @IncludeCookies@ , the values are automatically deleted.
--
-- * 'lcIncludeCookies' - Specifies whether you want CloudFront to include cookies in access logs, specify @true@ for @IncludeCookies@ . If you choose to include cookies in logs, CloudFront logs all cookies regardless of how you configure the cache behaviors for this distribution. If you don't want to include cookies when you create a distribution or if you want to disable include cookies for an existing distribution, specify @false@ for @IncludeCookies@ .
--
-- * 'lcBucket' - The Amazon S3 bucket to store the access logs in, for example, @myawslogbucket.s3.amazonaws.com@ .
--
-- * 'lcPrefix' - An optional string that you want CloudFront to prefix to the access log @filenames@ for this distribution, for example, @myprefix/@ . If you want to enable logging, but you don't want to specify a prefix, you still must include an empty @Prefix@ element in the @Logging@ element.
loggingConfig ::
  -- | 'lcEnabled'
  Bool ->
  -- | 'lcIncludeCookies'
  Bool ->
  -- | 'lcBucket'
  Text ->
  -- | 'lcPrefix'
  Text ->
  LoggingConfig
loggingConfig pEnabled_ pIncludeCookies_ pBucket_ pPrefix_ =
  LoggingConfig'
    { _lcEnabled = pEnabled_,
      _lcIncludeCookies = pIncludeCookies_,
      _lcBucket = pBucket_,
      _lcPrefix = pPrefix_
    }

-- | Specifies whether you want CloudFront to save access logs to an Amazon S3 bucket. If you don't want to enable logging when you create a distribution or if you want to disable logging for an existing distribution, specify @false@ for @Enabled@ , and specify empty @Bucket@ and @Prefix@ elements. If you specify @false@ for @Enabled@ but you specify values for @Bucket@ , @prefix@ , and @IncludeCookies@ , the values are automatically deleted.
lcEnabled :: Lens' LoggingConfig Bool
lcEnabled = lens _lcEnabled (\s a -> s {_lcEnabled = a})

-- | Specifies whether you want CloudFront to include cookies in access logs, specify @true@ for @IncludeCookies@ . If you choose to include cookies in logs, CloudFront logs all cookies regardless of how you configure the cache behaviors for this distribution. If you don't want to include cookies when you create a distribution or if you want to disable include cookies for an existing distribution, specify @false@ for @IncludeCookies@ .
lcIncludeCookies :: Lens' LoggingConfig Bool
lcIncludeCookies = lens _lcIncludeCookies (\s a -> s {_lcIncludeCookies = a})

-- | The Amazon S3 bucket to store the access logs in, for example, @myawslogbucket.s3.amazonaws.com@ .
lcBucket :: Lens' LoggingConfig Text
lcBucket = lens _lcBucket (\s a -> s {_lcBucket = a})

-- | An optional string that you want CloudFront to prefix to the access log @filenames@ for this distribution, for example, @myprefix/@ . If you want to enable logging, but you don't want to specify a prefix, you still must include an empty @Prefix@ element in the @Logging@ element.
lcPrefix :: Lens' LoggingConfig Text
lcPrefix = lens _lcPrefix (\s a -> s {_lcPrefix = a})

instance FromXML LoggingConfig where
  parseXML x =
    LoggingConfig'
      <$> (x .@ "Enabled")
      <*> (x .@ "IncludeCookies")
      <*> (x .@ "Bucket")
      <*> (x .@ "Prefix")

instance Hashable LoggingConfig

instance NFData LoggingConfig

instance ToXML LoggingConfig where
  toXML LoggingConfig' {..} =
    mconcat
      [ "Enabled" @= _lcEnabled,
        "IncludeCookies" @= _lcIncludeCookies,
        "Bucket" @= _lcBucket,
        "Prefix" @= _lcPrefix
      ]
