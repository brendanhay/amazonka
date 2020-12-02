{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53AutoNaming.Types.Namespace
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53AutoNaming.Types.Namespace where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Route53AutoNaming.Types.NamespaceProperties
import Network.AWS.Route53AutoNaming.Types.NamespaceType

-- | A complex type that contains information about a specified namespace.
--
--
--
-- /See:/ 'namespace' smart constructor.
data Namespace = Namespace'
  { _nARN :: !(Maybe Text),
    _nCreatorRequestId :: !(Maybe Text),
    _nCreateDate :: !(Maybe POSIX),
    _nServiceCount :: !(Maybe Int),
    _nName :: !(Maybe Text),
    _nId :: !(Maybe Text),
    _nType :: !(Maybe NamespaceType),
    _nDescription :: !(Maybe Text),
    _nProperties :: !(Maybe NamespaceProperties)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Namespace' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'nARN' - The Amazon Resource Name (ARN) that AWS Cloud Map assigns to the namespace when you create it.
--
-- * 'nCreatorRequestId' - A unique string that identifies the request and that allows failed requests to be retried without the risk of executing an operation twice.
--
-- * 'nCreateDate' - The date that the namespace was created, in Unix date/time format and Coordinated Universal Time (UTC). The value of @CreateDate@ is accurate to milliseconds. For example, the value @1516925490.087@ represents Friday, January 26, 2018 12:11:30.087 AM.
--
-- * 'nServiceCount' - The number of services that are associated with the namespace.
--
-- * 'nName' - The name of the namespace, such as @example.com@ .
--
-- * 'nId' - The ID of a namespace.
--
-- * 'nType' - The type of the namespace. The methods for discovering instances depends on the value that you specify:     * @HTTP@ : Instances can be discovered only programmatically, using the AWS Cloud Map @DiscoverInstances@ API.     * @DNS_PUBLIC@ : Instances can be discovered using public DNS queries and using the @DiscoverInstances@ API.     * @DNS_PRIVATE@ : Instances can be discovered using DNS queries in VPCs and using the @DiscoverInstances@ API.
--
-- * 'nDescription' - The description that you specify for the namespace when you create it.
--
-- * 'nProperties' - A complex type that contains information that's specific to the type of the namespace.
namespace ::
  Namespace
namespace =
  Namespace'
    { _nARN = Nothing,
      _nCreatorRequestId = Nothing,
      _nCreateDate = Nothing,
      _nServiceCount = Nothing,
      _nName = Nothing,
      _nId = Nothing,
      _nType = Nothing,
      _nDescription = Nothing,
      _nProperties = Nothing
    }

-- | The Amazon Resource Name (ARN) that AWS Cloud Map assigns to the namespace when you create it.
nARN :: Lens' Namespace (Maybe Text)
nARN = lens _nARN (\s a -> s {_nARN = a})

-- | A unique string that identifies the request and that allows failed requests to be retried without the risk of executing an operation twice.
nCreatorRequestId :: Lens' Namespace (Maybe Text)
nCreatorRequestId = lens _nCreatorRequestId (\s a -> s {_nCreatorRequestId = a})

-- | The date that the namespace was created, in Unix date/time format and Coordinated Universal Time (UTC). The value of @CreateDate@ is accurate to milliseconds. For example, the value @1516925490.087@ represents Friday, January 26, 2018 12:11:30.087 AM.
nCreateDate :: Lens' Namespace (Maybe UTCTime)
nCreateDate = lens _nCreateDate (\s a -> s {_nCreateDate = a}) . mapping _Time

-- | The number of services that are associated with the namespace.
nServiceCount :: Lens' Namespace (Maybe Int)
nServiceCount = lens _nServiceCount (\s a -> s {_nServiceCount = a})

-- | The name of the namespace, such as @example.com@ .
nName :: Lens' Namespace (Maybe Text)
nName = lens _nName (\s a -> s {_nName = a})

-- | The ID of a namespace.
nId :: Lens' Namespace (Maybe Text)
nId = lens _nId (\s a -> s {_nId = a})

-- | The type of the namespace. The methods for discovering instances depends on the value that you specify:     * @HTTP@ : Instances can be discovered only programmatically, using the AWS Cloud Map @DiscoverInstances@ API.     * @DNS_PUBLIC@ : Instances can be discovered using public DNS queries and using the @DiscoverInstances@ API.     * @DNS_PRIVATE@ : Instances can be discovered using DNS queries in VPCs and using the @DiscoverInstances@ API.
nType :: Lens' Namespace (Maybe NamespaceType)
nType = lens _nType (\s a -> s {_nType = a})

-- | The description that you specify for the namespace when you create it.
nDescription :: Lens' Namespace (Maybe Text)
nDescription = lens _nDescription (\s a -> s {_nDescription = a})

-- | A complex type that contains information that's specific to the type of the namespace.
nProperties :: Lens' Namespace (Maybe NamespaceProperties)
nProperties = lens _nProperties (\s a -> s {_nProperties = a})

instance FromJSON Namespace where
  parseJSON =
    withObject
      "Namespace"
      ( \x ->
          Namespace'
            <$> (x .:? "Arn")
            <*> (x .:? "CreatorRequestId")
            <*> (x .:? "CreateDate")
            <*> (x .:? "ServiceCount")
            <*> (x .:? "Name")
            <*> (x .:? "Id")
            <*> (x .:? "Type")
            <*> (x .:? "Description")
            <*> (x .:? "Properties")
      )

instance Hashable Namespace

instance NFData Namespace
