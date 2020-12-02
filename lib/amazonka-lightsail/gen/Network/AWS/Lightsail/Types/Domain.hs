{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.Domain
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.Domain where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types.DomainEntry
import Network.AWS.Lightsail.Types.ResourceLocation
import Network.AWS.Lightsail.Types.ResourceType
import Network.AWS.Lightsail.Types.Tag
import Network.AWS.Prelude

-- | Describes a domain where you are storing recordsets in Lightsail.
--
--
--
-- /See:/ 'domain' smart constructor.
data Domain = Domain'
  { _domResourceType :: !(Maybe ResourceType),
    _domDomainEntries :: !(Maybe [DomainEntry]),
    _domArn :: !(Maybe Text),
    _domCreatedAt :: !(Maybe POSIX),
    _domLocation :: !(Maybe ResourceLocation),
    _domName :: !(Maybe Text),
    _domSupportCode :: !(Maybe Text),
    _domTags :: !(Maybe [Tag])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Domain' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'domResourceType' - The resource type.
--
-- * 'domDomainEntries' - An array of key-value pairs containing information about the domain entries.
--
-- * 'domArn' - The Amazon Resource Name (ARN) of the domain recordset (e.g., @arn:aws:lightsail:global:123456789101:Domain/824cede0-abc7-4f84-8dbc-12345EXAMPLE@ ).
--
-- * 'domCreatedAt' - The date when the domain recordset was created.
--
-- * 'domLocation' - The AWS Region and Availability Zones where the domain recordset was created.
--
-- * 'domName' - The name of the domain.
--
-- * 'domSupportCode' - The support code. Include this code in your email to support when you have questions about an instance or another resource in Lightsail. This code enables our support team to look up your Lightsail information more easily.
--
-- * 'domTags' - The tag keys and optional values for the resource. For more information about tags in Lightsail, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide> .
domain ::
  Domain
domain =
  Domain'
    { _domResourceType = Nothing,
      _domDomainEntries = Nothing,
      _domArn = Nothing,
      _domCreatedAt = Nothing,
      _domLocation = Nothing,
      _domName = Nothing,
      _domSupportCode = Nothing,
      _domTags = Nothing
    }

-- | The resource type.
domResourceType :: Lens' Domain (Maybe ResourceType)
domResourceType = lens _domResourceType (\s a -> s {_domResourceType = a})

-- | An array of key-value pairs containing information about the domain entries.
domDomainEntries :: Lens' Domain [DomainEntry]
domDomainEntries = lens _domDomainEntries (\s a -> s {_domDomainEntries = a}) . _Default . _Coerce

-- | The Amazon Resource Name (ARN) of the domain recordset (e.g., @arn:aws:lightsail:global:123456789101:Domain/824cede0-abc7-4f84-8dbc-12345EXAMPLE@ ).
domArn :: Lens' Domain (Maybe Text)
domArn = lens _domArn (\s a -> s {_domArn = a})

-- | The date when the domain recordset was created.
domCreatedAt :: Lens' Domain (Maybe UTCTime)
domCreatedAt = lens _domCreatedAt (\s a -> s {_domCreatedAt = a}) . mapping _Time

-- | The AWS Region and Availability Zones where the domain recordset was created.
domLocation :: Lens' Domain (Maybe ResourceLocation)
domLocation = lens _domLocation (\s a -> s {_domLocation = a})

-- | The name of the domain.
domName :: Lens' Domain (Maybe Text)
domName = lens _domName (\s a -> s {_domName = a})

-- | The support code. Include this code in your email to support when you have questions about an instance or another resource in Lightsail. This code enables our support team to look up your Lightsail information more easily.
domSupportCode :: Lens' Domain (Maybe Text)
domSupportCode = lens _domSupportCode (\s a -> s {_domSupportCode = a})

-- | The tag keys and optional values for the resource. For more information about tags in Lightsail, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide> .
domTags :: Lens' Domain [Tag]
domTags = lens _domTags (\s a -> s {_domTags = a}) . _Default . _Coerce

instance FromJSON Domain where
  parseJSON =
    withObject
      "Domain"
      ( \x ->
          Domain'
            <$> (x .:? "resourceType")
            <*> (x .:? "domainEntries" .!= mempty)
            <*> (x .:? "arn")
            <*> (x .:? "createdAt")
            <*> (x .:? "location")
            <*> (x .:? "name")
            <*> (x .:? "supportCode")
            <*> (x .:? "tags" .!= mempty)
      )

instance Hashable Domain

instance NFData Domain
