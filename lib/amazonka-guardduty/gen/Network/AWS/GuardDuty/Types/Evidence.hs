{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.Evidence
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.Evidence where

import Network.AWS.GuardDuty.Types.ThreatIntelligenceDetail
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information about the reason that the finding was generated.
--
--
--
-- /See:/ 'evidence' smart constructor.
newtype Evidence = Evidence'
  { _eThreatIntelligenceDetails ::
      Maybe [ThreatIntelligenceDetail]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Evidence' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eThreatIntelligenceDetails' - A list of threat intelligence details related to the evidence.
evidence ::
  Evidence
evidence = Evidence' {_eThreatIntelligenceDetails = Nothing}

-- | A list of threat intelligence details related to the evidence.
eThreatIntelligenceDetails :: Lens' Evidence [ThreatIntelligenceDetail]
eThreatIntelligenceDetails = lens _eThreatIntelligenceDetails (\s a -> s {_eThreatIntelligenceDetails = a}) . _Default . _Coerce

instance FromJSON Evidence where
  parseJSON =
    withObject
      "Evidence"
      ( \x ->
          Evidence' <$> (x .:? "threatIntelligenceDetails" .!= mempty)
      )

instance Hashable Evidence

instance NFData Evidence
