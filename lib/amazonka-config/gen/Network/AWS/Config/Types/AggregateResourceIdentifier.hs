{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.AggregateResourceIdentifier
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.AggregateResourceIdentifier where

import Network.AWS.Config.Types.ResourceType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The details that identify a resource that is collected by AWS Config aggregator, including the resource type, ID, (if available) the custom resource name, the source account, and source region.
--
--
--
-- /See:/ 'aggregateResourceIdentifier' smart constructor.
data AggregateResourceIdentifier = AggregateResourceIdentifier'
  { _ariResourceName ::
      !(Maybe Text),
    _ariSourceAccountId :: !Text,
    _ariSourceRegion :: !Text,
    _ariResourceId :: !Text,
    _ariResourceType :: !ResourceType
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AggregateResourceIdentifier' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ariResourceName' - The name of the AWS resource.
--
-- * 'ariSourceAccountId' - The 12-digit account ID of the source account.
--
-- * 'ariSourceRegion' - The source region where data is aggregated.
--
-- * 'ariResourceId' - The ID of the AWS resource.
--
-- * 'ariResourceType' - The type of the AWS resource.
aggregateResourceIdentifier ::
  -- | 'ariSourceAccountId'
  Text ->
  -- | 'ariSourceRegion'
  Text ->
  -- | 'ariResourceId'
  Text ->
  -- | 'ariResourceType'
  ResourceType ->
  AggregateResourceIdentifier
aggregateResourceIdentifier
  pSourceAccountId_
  pSourceRegion_
  pResourceId_
  pResourceType_ =
    AggregateResourceIdentifier'
      { _ariResourceName = Nothing,
        _ariSourceAccountId = pSourceAccountId_,
        _ariSourceRegion = pSourceRegion_,
        _ariResourceId = pResourceId_,
        _ariResourceType = pResourceType_
      }

-- | The name of the AWS resource.
ariResourceName :: Lens' AggregateResourceIdentifier (Maybe Text)
ariResourceName = lens _ariResourceName (\s a -> s {_ariResourceName = a})

-- | The 12-digit account ID of the source account.
ariSourceAccountId :: Lens' AggregateResourceIdentifier Text
ariSourceAccountId = lens _ariSourceAccountId (\s a -> s {_ariSourceAccountId = a})

-- | The source region where data is aggregated.
ariSourceRegion :: Lens' AggregateResourceIdentifier Text
ariSourceRegion = lens _ariSourceRegion (\s a -> s {_ariSourceRegion = a})

-- | The ID of the AWS resource.
ariResourceId :: Lens' AggregateResourceIdentifier Text
ariResourceId = lens _ariResourceId (\s a -> s {_ariResourceId = a})

-- | The type of the AWS resource.
ariResourceType :: Lens' AggregateResourceIdentifier ResourceType
ariResourceType = lens _ariResourceType (\s a -> s {_ariResourceType = a})

instance FromJSON AggregateResourceIdentifier where
  parseJSON =
    withObject
      "AggregateResourceIdentifier"
      ( \x ->
          AggregateResourceIdentifier'
            <$> (x .:? "ResourceName")
            <*> (x .: "SourceAccountId")
            <*> (x .: "SourceRegion")
            <*> (x .: "ResourceId")
            <*> (x .: "ResourceType")
      )

instance Hashable AggregateResourceIdentifier

instance NFData AggregateResourceIdentifier

instance ToJSON AggregateResourceIdentifier where
  toJSON AggregateResourceIdentifier' {..} =
    object
      ( catMaybes
          [ ("ResourceName" .=) <$> _ariResourceName,
            Just ("SourceAccountId" .= _ariSourceAccountId),
            Just ("SourceRegion" .= _ariSourceRegion),
            Just ("ResourceId" .= _ariResourceId),
            Just ("ResourceType" .= _ariResourceType)
          ]
      )
