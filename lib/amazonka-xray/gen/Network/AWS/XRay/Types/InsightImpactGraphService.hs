{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types.InsightImpactGraphService
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.InsightImpactGraphService where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.XRay.Types.InsightImpactGraphEdge

-- | Information about an application that processed requests, users that made requests, or downstream services, resources, and applications that an application used.
--
--
--
-- /See:/ 'insightImpactGraphService' smart constructor.
data InsightImpactGraphService = InsightImpactGraphService'
  { _iigsReferenceId ::
      !(Maybe Int),
    _iigsAccountId :: !(Maybe Text),
    _iigsNames :: !(Maybe [Text]),
    _iigsName :: !(Maybe Text),
    _iigsType :: !(Maybe Text),
    _iigsEdges ::
      !(Maybe [InsightImpactGraphEdge])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InsightImpactGraphService' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iigsReferenceId' - Identifier for the service. Unique within the service map.
--
-- * 'iigsAccountId' - Identifier of the AWS account in which the service runs.
--
-- * 'iigsNames' - A list of names for the service, including the canonical name.
--
-- * 'iigsName' - The canonical name of the service.
--
-- * 'iigsType' - Identifier for the service. Unique within the service map.     * AWS Resource - The type of an AWS resource. For example, AWS::EC2::Instance for an application running on Amazon EC2 or AWS::DynamoDB::Table for an Amazon DynamoDB table that the application used.      * AWS Service - The type of an AWS service. For example, AWS::DynamoDB for downstream calls to Amazon DynamoDB that didn't target a specific table.      * AWS Service - The type of an AWS service. For example, AWS::DynamoDB for downstream calls to Amazon DynamoDB that didn't target a specific table.      * remote - A downstream service of indeterminate type.
--
-- * 'iigsEdges' - Connections to downstream services.
insightImpactGraphService ::
  InsightImpactGraphService
insightImpactGraphService =
  InsightImpactGraphService'
    { _iigsReferenceId = Nothing,
      _iigsAccountId = Nothing,
      _iigsNames = Nothing,
      _iigsName = Nothing,
      _iigsType = Nothing,
      _iigsEdges = Nothing
    }

-- | Identifier for the service. Unique within the service map.
iigsReferenceId :: Lens' InsightImpactGraphService (Maybe Int)
iigsReferenceId = lens _iigsReferenceId (\s a -> s {_iigsReferenceId = a})

-- | Identifier of the AWS account in which the service runs.
iigsAccountId :: Lens' InsightImpactGraphService (Maybe Text)
iigsAccountId = lens _iigsAccountId (\s a -> s {_iigsAccountId = a})

-- | A list of names for the service, including the canonical name.
iigsNames :: Lens' InsightImpactGraphService [Text]
iigsNames = lens _iigsNames (\s a -> s {_iigsNames = a}) . _Default . _Coerce

-- | The canonical name of the service.
iigsName :: Lens' InsightImpactGraphService (Maybe Text)
iigsName = lens _iigsName (\s a -> s {_iigsName = a})

-- | Identifier for the service. Unique within the service map.     * AWS Resource - The type of an AWS resource. For example, AWS::EC2::Instance for an application running on Amazon EC2 or AWS::DynamoDB::Table for an Amazon DynamoDB table that the application used.      * AWS Service - The type of an AWS service. For example, AWS::DynamoDB for downstream calls to Amazon DynamoDB that didn't target a specific table.      * AWS Service - The type of an AWS service. For example, AWS::DynamoDB for downstream calls to Amazon DynamoDB that didn't target a specific table.      * remote - A downstream service of indeterminate type.
iigsType :: Lens' InsightImpactGraphService (Maybe Text)
iigsType = lens _iigsType (\s a -> s {_iigsType = a})

-- | Connections to downstream services.
iigsEdges :: Lens' InsightImpactGraphService [InsightImpactGraphEdge]
iigsEdges = lens _iigsEdges (\s a -> s {_iigsEdges = a}) . _Default . _Coerce

instance FromJSON InsightImpactGraphService where
  parseJSON =
    withObject
      "InsightImpactGraphService"
      ( \x ->
          InsightImpactGraphService'
            <$> (x .:? "ReferenceId")
            <*> (x .:? "AccountId")
            <*> (x .:? "Names" .!= mempty)
            <*> (x .:? "Name")
            <*> (x .:? "Type")
            <*> (x .:? "Edges" .!= mempty)
      )

instance Hashable InsightImpactGraphService

instance NFData InsightImpactGraphService
