{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types.InsightImpactGraphEdge
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.InsightImpactGraphEdge where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The connection between two service in an insight impact graph.
--
--
--
-- /See:/ 'insightImpactGraphEdge' smart constructor.
newtype InsightImpactGraphEdge = InsightImpactGraphEdge'
  { _iigeReferenceId ::
      Maybe Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InsightImpactGraphEdge' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iigeReferenceId' - Identifier of the edge. Unique within a service map.
insightImpactGraphEdge ::
  InsightImpactGraphEdge
insightImpactGraphEdge =
  InsightImpactGraphEdge' {_iigeReferenceId = Nothing}

-- | Identifier of the edge. Unique within a service map.
iigeReferenceId :: Lens' InsightImpactGraphEdge (Maybe Int)
iigeReferenceId = lens _iigeReferenceId (\s a -> s {_iigeReferenceId = a})

instance FromJSON InsightImpactGraphEdge where
  parseJSON =
    withObject
      "InsightImpactGraphEdge"
      (\x -> InsightImpactGraphEdge' <$> (x .:? "ReferenceId"))

instance Hashable InsightImpactGraphEdge

instance NFData InsightImpactGraphEdge
