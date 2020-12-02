{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.PiiOutputDataConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.PiiOutputDataConfig where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides configuration parameters for the output of PII entity detection jobs.
--
--
--
-- /See:/ 'piiOutputDataConfig' smart constructor.
data PiiOutputDataConfig = PiiOutputDataConfig'
  { _podcKMSKeyId ::
      !(Maybe Text),
    _podcS3URI :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PiiOutputDataConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'podcKMSKeyId' - ID for the AWS Key Management Service (KMS) key that Amazon Comprehend uses to encrypt the output results from an analysis job.
--
-- * 'podcS3URI' - When you use the @PiiOutputDataConfig@ object with asynchronous operations, you specify the Amazon S3 location where you want to write the output data.
piiOutputDataConfig ::
  -- | 'podcS3URI'
  Text ->
  PiiOutputDataConfig
piiOutputDataConfig pS3URI_ =
  PiiOutputDataConfig'
    { _podcKMSKeyId = Nothing,
      _podcS3URI = pS3URI_
    }

-- | ID for the AWS Key Management Service (KMS) key that Amazon Comprehend uses to encrypt the output results from an analysis job.
podcKMSKeyId :: Lens' PiiOutputDataConfig (Maybe Text)
podcKMSKeyId = lens _podcKMSKeyId (\s a -> s {_podcKMSKeyId = a})

-- | When you use the @PiiOutputDataConfig@ object with asynchronous operations, you specify the Amazon S3 location where you want to write the output data.
podcS3URI :: Lens' PiiOutputDataConfig Text
podcS3URI = lens _podcS3URI (\s a -> s {_podcS3URI = a})

instance FromJSON PiiOutputDataConfig where
  parseJSON =
    withObject
      "PiiOutputDataConfig"
      ( \x ->
          PiiOutputDataConfig' <$> (x .:? "KmsKeyId") <*> (x .: "S3Uri")
      )

instance Hashable PiiOutputDataConfig

instance NFData PiiOutputDataConfig
