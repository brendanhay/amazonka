{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.ThreatIntelligenceDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.ThreatIntelligenceDetail where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | An instance of a threat intelligence detail that constitutes evidence for the finding.
--
--
--
-- /See:/ 'threatIntelligenceDetail' smart constructor.
data ThreatIntelligenceDetail = ThreatIntelligenceDetail'
  { _tidThreatNames ::
      !(Maybe [Text]),
    _tidThreatListName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ThreatIntelligenceDetail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tidThreatNames' - A list of names of the threats in the threat intelligence list that triggered the finding.
--
-- * 'tidThreatListName' - The name of the threat intelligence list that triggered the finding.
threatIntelligenceDetail ::
  ThreatIntelligenceDetail
threatIntelligenceDetail =
  ThreatIntelligenceDetail'
    { _tidThreatNames = Nothing,
      _tidThreatListName = Nothing
    }

-- | A list of names of the threats in the threat intelligence list that triggered the finding.
tidThreatNames :: Lens' ThreatIntelligenceDetail [Text]
tidThreatNames = lens _tidThreatNames (\s a -> s {_tidThreatNames = a}) . _Default . _Coerce

-- | The name of the threat intelligence list that triggered the finding.
tidThreatListName :: Lens' ThreatIntelligenceDetail (Maybe Text)
tidThreatListName = lens _tidThreatListName (\s a -> s {_tidThreatListName = a})

instance FromJSON ThreatIntelligenceDetail where
  parseJSON =
    withObject
      "ThreatIntelligenceDetail"
      ( \x ->
          ThreatIntelligenceDetail'
            <$> (x .:? "threatNames" .!= mempty) <*> (x .:? "threatListName")
      )

instance Hashable ThreatIntelligenceDetail

instance NFData ThreatIntelligenceDetail
