{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.PublicKey
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.PublicKey where

import Network.AWS.CloudFront.Types.PublicKeyConfig
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A public key that you can use with <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PrivateContent.html signed URLs and signed cookies> , or with <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/field-level-encryption.html field-level encryption> .
--
--
--
-- /See:/ 'publicKey' smart constructor.
data PublicKey = PublicKey'
  { _pkId :: !Text,
    _pkCreatedTime :: !ISO8601,
    _pkPublicKeyConfig :: !PublicKeyConfig
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PublicKey' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pkId' - The identifier of the public key.
--
-- * 'pkCreatedTime' - The date and time when the public key was uploaded.
--
-- * 'pkPublicKeyConfig' - Configuration information about a public key that you can use with <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PrivateContent.html signed URLs and signed cookies> , or with <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/field-level-encryption.html field-level encryption> .
publicKey ::
  -- | 'pkId'
  Text ->
  -- | 'pkCreatedTime'
  UTCTime ->
  -- | 'pkPublicKeyConfig'
  PublicKeyConfig ->
  PublicKey
publicKey pId_ pCreatedTime_ pPublicKeyConfig_ =
  PublicKey'
    { _pkId = pId_,
      _pkCreatedTime = _Time # pCreatedTime_,
      _pkPublicKeyConfig = pPublicKeyConfig_
    }

-- | The identifier of the public key.
pkId :: Lens' PublicKey Text
pkId = lens _pkId (\s a -> s {_pkId = a})

-- | The date and time when the public key was uploaded.
pkCreatedTime :: Lens' PublicKey UTCTime
pkCreatedTime = lens _pkCreatedTime (\s a -> s {_pkCreatedTime = a}) . _Time

-- | Configuration information about a public key that you can use with <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PrivateContent.html signed URLs and signed cookies> , or with <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/field-level-encryption.html field-level encryption> .
pkPublicKeyConfig :: Lens' PublicKey PublicKeyConfig
pkPublicKeyConfig = lens _pkPublicKeyConfig (\s a -> s {_pkPublicKeyConfig = a})

instance FromXML PublicKey where
  parseXML x =
    PublicKey'
      <$> (x .@ "Id") <*> (x .@ "CreatedTime") <*> (x .@ "PublicKeyConfig")

instance Hashable PublicKey

instance NFData PublicKey
