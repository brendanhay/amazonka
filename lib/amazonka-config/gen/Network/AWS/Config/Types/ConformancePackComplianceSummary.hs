{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.ConformancePackComplianceSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.ConformancePackComplianceSummary where

import Network.AWS.Config.Types.ConformancePackComplianceType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Summary includes the name and status of the conformance pack.
--
--
--
-- /See:/ 'conformancePackComplianceSummary' smart constructor.
data ConformancePackComplianceSummary = ConformancePackComplianceSummary'
  { _cpcsConformancePackName ::
      !Text,
    _cpcsConformancePackComplianceStatus ::
      !ConformancePackComplianceType
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ConformancePackComplianceSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cpcsConformancePackName' - The name of the conformance pack name.
--
-- * 'cpcsConformancePackComplianceStatus' - The status of the conformance pack. The allowed values are COMPLIANT and NON_COMPLIANT.
conformancePackComplianceSummary ::
  -- | 'cpcsConformancePackName'
  Text ->
  -- | 'cpcsConformancePackComplianceStatus'
  ConformancePackComplianceType ->
  ConformancePackComplianceSummary
conformancePackComplianceSummary
  pConformancePackName_
  pConformancePackComplianceStatus_ =
    ConformancePackComplianceSummary'
      { _cpcsConformancePackName =
          pConformancePackName_,
        _cpcsConformancePackComplianceStatus =
          pConformancePackComplianceStatus_
      }

-- | The name of the conformance pack name.
cpcsConformancePackName :: Lens' ConformancePackComplianceSummary Text
cpcsConformancePackName = lens _cpcsConformancePackName (\s a -> s {_cpcsConformancePackName = a})

-- | The status of the conformance pack. The allowed values are COMPLIANT and NON_COMPLIANT.
cpcsConformancePackComplianceStatus :: Lens' ConformancePackComplianceSummary ConformancePackComplianceType
cpcsConformancePackComplianceStatus = lens _cpcsConformancePackComplianceStatus (\s a -> s {_cpcsConformancePackComplianceStatus = a})

instance FromJSON ConformancePackComplianceSummary where
  parseJSON =
    withObject
      "ConformancePackComplianceSummary"
      ( \x ->
          ConformancePackComplianceSummary'
            <$> (x .: "ConformancePackName")
            <*> (x .: "ConformancePackComplianceStatus")
      )

instance Hashable ConformancePackComplianceSummary

instance NFData ConformancePackComplianceSummary
