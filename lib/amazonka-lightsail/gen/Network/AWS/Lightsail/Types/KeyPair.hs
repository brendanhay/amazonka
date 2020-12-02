{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.KeyPair
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.KeyPair where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types.ResourceLocation
import Network.AWS.Lightsail.Types.ResourceType
import Network.AWS.Lightsail.Types.Tag
import Network.AWS.Prelude

-- | Describes the SSH key pair.
--
--
--
-- /See:/ 'keyPair' smart constructor.
data KeyPair = KeyPair'
  { _kpResourceType :: !(Maybe ResourceType),
    _kpArn :: !(Maybe Text),
    _kpCreatedAt :: !(Maybe POSIX),
    _kpLocation :: !(Maybe ResourceLocation),
    _kpFingerprint :: !(Maybe Text),
    _kpName :: !(Maybe Text),
    _kpSupportCode :: !(Maybe Text),
    _kpTags :: !(Maybe [Tag])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'KeyPair' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'kpResourceType' - The resource type (usually @KeyPair@ ).
--
-- * 'kpArn' - The Amazon Resource Name (ARN) of the key pair (e.g., @arn:aws:lightsail:us-east-2:123456789101:KeyPair/05859e3d-331d-48ba-9034-12345EXAMPLE@ ).
--
-- * 'kpCreatedAt' - The timestamp when the key pair was created (e.g., @1479816991.349@ ).
--
-- * 'kpLocation' - The region name and Availability Zone where the key pair was created.
--
-- * 'kpFingerprint' - The RSA fingerprint of the key pair.
--
-- * 'kpName' - The friendly name of the SSH key pair.
--
-- * 'kpSupportCode' - The support code. Include this code in your email to support when you have questions about an instance or another resource in Lightsail. This code enables our support team to look up your Lightsail information more easily.
--
-- * 'kpTags' - The tag keys and optional values for the resource. For more information about tags in Lightsail, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide> .
keyPair ::
  KeyPair
keyPair =
  KeyPair'
    { _kpResourceType = Nothing,
      _kpArn = Nothing,
      _kpCreatedAt = Nothing,
      _kpLocation = Nothing,
      _kpFingerprint = Nothing,
      _kpName = Nothing,
      _kpSupportCode = Nothing,
      _kpTags = Nothing
    }

-- | The resource type (usually @KeyPair@ ).
kpResourceType :: Lens' KeyPair (Maybe ResourceType)
kpResourceType = lens _kpResourceType (\s a -> s {_kpResourceType = a})

-- | The Amazon Resource Name (ARN) of the key pair (e.g., @arn:aws:lightsail:us-east-2:123456789101:KeyPair/05859e3d-331d-48ba-9034-12345EXAMPLE@ ).
kpArn :: Lens' KeyPair (Maybe Text)
kpArn = lens _kpArn (\s a -> s {_kpArn = a})

-- | The timestamp when the key pair was created (e.g., @1479816991.349@ ).
kpCreatedAt :: Lens' KeyPair (Maybe UTCTime)
kpCreatedAt = lens _kpCreatedAt (\s a -> s {_kpCreatedAt = a}) . mapping _Time

-- | The region name and Availability Zone where the key pair was created.
kpLocation :: Lens' KeyPair (Maybe ResourceLocation)
kpLocation = lens _kpLocation (\s a -> s {_kpLocation = a})

-- | The RSA fingerprint of the key pair.
kpFingerprint :: Lens' KeyPair (Maybe Text)
kpFingerprint = lens _kpFingerprint (\s a -> s {_kpFingerprint = a})

-- | The friendly name of the SSH key pair.
kpName :: Lens' KeyPair (Maybe Text)
kpName = lens _kpName (\s a -> s {_kpName = a})

-- | The support code. Include this code in your email to support when you have questions about an instance or another resource in Lightsail. This code enables our support team to look up your Lightsail information more easily.
kpSupportCode :: Lens' KeyPair (Maybe Text)
kpSupportCode = lens _kpSupportCode (\s a -> s {_kpSupportCode = a})

-- | The tag keys and optional values for the resource. For more information about tags in Lightsail, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide> .
kpTags :: Lens' KeyPair [Tag]
kpTags = lens _kpTags (\s a -> s {_kpTags = a}) . _Default . _Coerce

instance FromJSON KeyPair where
  parseJSON =
    withObject
      "KeyPair"
      ( \x ->
          KeyPair'
            <$> (x .:? "resourceType")
            <*> (x .:? "arn")
            <*> (x .:? "createdAt")
            <*> (x .:? "location")
            <*> (x .:? "fingerprint")
            <*> (x .:? "name")
            <*> (x .:? "supportCode")
            <*> (x .:? "tags" .!= mempty)
      )

instance Hashable KeyPair

instance NFData KeyPair
