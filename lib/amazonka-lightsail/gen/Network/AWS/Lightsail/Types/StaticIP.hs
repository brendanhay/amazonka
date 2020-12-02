{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.StaticIP
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.StaticIP where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types.ResourceLocation
import Network.AWS.Lightsail.Types.ResourceType
import Network.AWS.Prelude

-- | Describes the static IP.
--
--
--
-- /See:/ 'staticIP' smart constructor.
data StaticIP = StaticIP'
  { _siIpAddress :: !(Maybe Text),
    _siResourceType :: !(Maybe ResourceType),
    _siArn :: !(Maybe Text),
    _siCreatedAt :: !(Maybe POSIX),
    _siLocation :: !(Maybe ResourceLocation),
    _siIsAttached :: !(Maybe Bool),
    _siName :: !(Maybe Text),
    _siSupportCode :: !(Maybe Text),
    _siAttachedTo :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StaticIP' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'siIpAddress' - The static IP address.
--
-- * 'siResourceType' - The resource type (usually @StaticIp@ ).
--
-- * 'siArn' - The Amazon Resource Name (ARN) of the static IP (e.g., @arn:aws:lightsail:us-east-2:123456789101:StaticIp/9cbb4a9e-f8e3-4dfe-b57e-12345EXAMPLE@ ).
--
-- * 'siCreatedAt' - The timestamp when the static IP was created (e.g., @1479735304.222@ ).
--
-- * 'siLocation' - The region and Availability Zone where the static IP was created.
--
-- * 'siIsAttached' - A Boolean value indicating whether the static IP is attached.
--
-- * 'siName' - The name of the static IP (e.g., @StaticIP-Ohio-EXAMPLE@ ).
--
-- * 'siSupportCode' - The support code. Include this code in your email to support when you have questions about an instance or another resource in Lightsail. This code enables our support team to look up your Lightsail information more easily.
--
-- * 'siAttachedTo' - The instance where the static IP is attached (e.g., @Amazon_Linux-1GB-Ohio-1@ ).
staticIP ::
  StaticIP
staticIP =
  StaticIP'
    { _siIpAddress = Nothing,
      _siResourceType = Nothing,
      _siArn = Nothing,
      _siCreatedAt = Nothing,
      _siLocation = Nothing,
      _siIsAttached = Nothing,
      _siName = Nothing,
      _siSupportCode = Nothing,
      _siAttachedTo = Nothing
    }

-- | The static IP address.
siIpAddress :: Lens' StaticIP (Maybe Text)
siIpAddress = lens _siIpAddress (\s a -> s {_siIpAddress = a})

-- | The resource type (usually @StaticIp@ ).
siResourceType :: Lens' StaticIP (Maybe ResourceType)
siResourceType = lens _siResourceType (\s a -> s {_siResourceType = a})

-- | The Amazon Resource Name (ARN) of the static IP (e.g., @arn:aws:lightsail:us-east-2:123456789101:StaticIp/9cbb4a9e-f8e3-4dfe-b57e-12345EXAMPLE@ ).
siArn :: Lens' StaticIP (Maybe Text)
siArn = lens _siArn (\s a -> s {_siArn = a})

-- | The timestamp when the static IP was created (e.g., @1479735304.222@ ).
siCreatedAt :: Lens' StaticIP (Maybe UTCTime)
siCreatedAt = lens _siCreatedAt (\s a -> s {_siCreatedAt = a}) . mapping _Time

-- | The region and Availability Zone where the static IP was created.
siLocation :: Lens' StaticIP (Maybe ResourceLocation)
siLocation = lens _siLocation (\s a -> s {_siLocation = a})

-- | A Boolean value indicating whether the static IP is attached.
siIsAttached :: Lens' StaticIP (Maybe Bool)
siIsAttached = lens _siIsAttached (\s a -> s {_siIsAttached = a})

-- | The name of the static IP (e.g., @StaticIP-Ohio-EXAMPLE@ ).
siName :: Lens' StaticIP (Maybe Text)
siName = lens _siName (\s a -> s {_siName = a})

-- | The support code. Include this code in your email to support when you have questions about an instance or another resource in Lightsail. This code enables our support team to look up your Lightsail information more easily.
siSupportCode :: Lens' StaticIP (Maybe Text)
siSupportCode = lens _siSupportCode (\s a -> s {_siSupportCode = a})

-- | The instance where the static IP is attached (e.g., @Amazon_Linux-1GB-Ohio-1@ ).
siAttachedTo :: Lens' StaticIP (Maybe Text)
siAttachedTo = lens _siAttachedTo (\s a -> s {_siAttachedTo = a})

instance FromJSON StaticIP where
  parseJSON =
    withObject
      "StaticIP"
      ( \x ->
          StaticIP'
            <$> (x .:? "ipAddress")
            <*> (x .:? "resourceType")
            <*> (x .:? "arn")
            <*> (x .:? "createdAt")
            <*> (x .:? "location")
            <*> (x .:? "isAttached")
            <*> (x .:? "name")
            <*> (x .:? "supportCode")
            <*> (x .:? "attachedTo")
      )

instance Hashable StaticIP

instance NFData StaticIP
