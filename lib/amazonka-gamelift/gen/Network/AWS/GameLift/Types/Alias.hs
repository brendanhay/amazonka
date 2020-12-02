{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.Alias
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.Alias where

import Network.AWS.GameLift.Types.RoutingStrategy
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Properties that describe an alias resource.
--
--
--     * 'CreateAlias'
--
--     * 'ListAliases'
--
--     * 'DescribeAlias'
--
--     * 'UpdateAlias'
--
--     * 'DeleteAlias'
--
--     * 'ResolveAlias'
--
--
--
--
-- /See:/ 'alias' smart constructor.
data Alias = Alias'
  { _aCreationTime :: !(Maybe POSIX),
    _aLastUpdatedTime :: !(Maybe POSIX),
    _aAliasId :: !(Maybe Text),
    _aRoutingStrategy :: !(Maybe RoutingStrategy),
    _aName :: !(Maybe Text),
    _aAliasARN :: !(Maybe Text),
    _aDescription :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Alias' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aCreationTime' - A time stamp indicating when this data object was created. Format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
--
-- * 'aLastUpdatedTime' - The time that this data object was last modified. Format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
--
-- * 'aAliasId' - A unique identifier for an alias. Alias IDs are unique within a Region.
--
-- * 'aRoutingStrategy' - The routing configuration, including routing type and fleet target, for the alias.
--
-- * 'aName' - A descriptive label that is associated with an alias. Alias names do not need to be unique.
--
-- * 'aAliasARN' - Amazon Resource Name (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN> ) that is assigned to a GameLift alias resource and uniquely identifies it. ARNs are unique across all Regions. In a GameLift alias ARN, the resource ID matches the alias ID value.
--
-- * 'aDescription' - A human-readable description of an alias.
alias ::
  Alias
alias =
  Alias'
    { _aCreationTime = Nothing,
      _aLastUpdatedTime = Nothing,
      _aAliasId = Nothing,
      _aRoutingStrategy = Nothing,
      _aName = Nothing,
      _aAliasARN = Nothing,
      _aDescription = Nothing
    }

-- | A time stamp indicating when this data object was created. Format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
aCreationTime :: Lens' Alias (Maybe UTCTime)
aCreationTime = lens _aCreationTime (\s a -> s {_aCreationTime = a}) . mapping _Time

-- | The time that this data object was last modified. Format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
aLastUpdatedTime :: Lens' Alias (Maybe UTCTime)
aLastUpdatedTime = lens _aLastUpdatedTime (\s a -> s {_aLastUpdatedTime = a}) . mapping _Time

-- | A unique identifier for an alias. Alias IDs are unique within a Region.
aAliasId :: Lens' Alias (Maybe Text)
aAliasId = lens _aAliasId (\s a -> s {_aAliasId = a})

-- | The routing configuration, including routing type and fleet target, for the alias.
aRoutingStrategy :: Lens' Alias (Maybe RoutingStrategy)
aRoutingStrategy = lens _aRoutingStrategy (\s a -> s {_aRoutingStrategy = a})

-- | A descriptive label that is associated with an alias. Alias names do not need to be unique.
aName :: Lens' Alias (Maybe Text)
aName = lens _aName (\s a -> s {_aName = a})

-- | Amazon Resource Name (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN> ) that is assigned to a GameLift alias resource and uniquely identifies it. ARNs are unique across all Regions. In a GameLift alias ARN, the resource ID matches the alias ID value.
aAliasARN :: Lens' Alias (Maybe Text)
aAliasARN = lens _aAliasARN (\s a -> s {_aAliasARN = a})

-- | A human-readable description of an alias.
aDescription :: Lens' Alias (Maybe Text)
aDescription = lens _aDescription (\s a -> s {_aDescription = a})

instance FromJSON Alias where
  parseJSON =
    withObject
      "Alias"
      ( \x ->
          Alias'
            <$> (x .:? "CreationTime")
            <*> (x .:? "LastUpdatedTime")
            <*> (x .:? "AliasId")
            <*> (x .:? "RoutingStrategy")
            <*> (x .:? "Name")
            <*> (x .:? "AliasArn")
            <*> (x .:? "Description")
      )

instance Hashable Alias

instance NFData Alias
