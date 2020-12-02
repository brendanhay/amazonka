{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.SeveritySummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.SeveritySummary where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The number of managed instances found for each patch severity level defined in the request filter.
--
--
--
-- /See:/ 'severitySummary' smart constructor.
data SeveritySummary = SeveritySummary'
  { _ssLowCount ::
      !(Maybe Int),
    _ssUnspecifiedCount :: !(Maybe Int),
    _ssHighCount :: !(Maybe Int),
    _ssMediumCount :: !(Maybe Int),
    _ssInformationalCount :: !(Maybe Int),
    _ssCriticalCount :: !(Maybe Int)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SeveritySummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ssLowCount' - The total number of resources or compliance items that have a severity level of low. Low severity is determined by the organization that published the compliance items.
--
-- * 'ssUnspecifiedCount' - The total number of resources or compliance items that have a severity level of unspecified. Unspecified severity is determined by the organization that published the compliance items.
--
-- * 'ssHighCount' - The total number of resources or compliance items that have a severity level of high. High severity is determined by the organization that published the compliance items.
--
-- * 'ssMediumCount' - The total number of resources or compliance items that have a severity level of medium. Medium severity is determined by the organization that published the compliance items.
--
-- * 'ssInformationalCount' - The total number of resources or compliance items that have a severity level of informational. Informational severity is determined by the organization that published the compliance items.
--
-- * 'ssCriticalCount' - The total number of resources or compliance items that have a severity level of critical. Critical severity is determined by the organization that published the compliance items.
severitySummary ::
  SeveritySummary
severitySummary =
  SeveritySummary'
    { _ssLowCount = Nothing,
      _ssUnspecifiedCount = Nothing,
      _ssHighCount = Nothing,
      _ssMediumCount = Nothing,
      _ssInformationalCount = Nothing,
      _ssCriticalCount = Nothing
    }

-- | The total number of resources or compliance items that have a severity level of low. Low severity is determined by the organization that published the compliance items.
ssLowCount :: Lens' SeveritySummary (Maybe Int)
ssLowCount = lens _ssLowCount (\s a -> s {_ssLowCount = a})

-- | The total number of resources or compliance items that have a severity level of unspecified. Unspecified severity is determined by the organization that published the compliance items.
ssUnspecifiedCount :: Lens' SeveritySummary (Maybe Int)
ssUnspecifiedCount = lens _ssUnspecifiedCount (\s a -> s {_ssUnspecifiedCount = a})

-- | The total number of resources or compliance items that have a severity level of high. High severity is determined by the organization that published the compliance items.
ssHighCount :: Lens' SeveritySummary (Maybe Int)
ssHighCount = lens _ssHighCount (\s a -> s {_ssHighCount = a})

-- | The total number of resources or compliance items that have a severity level of medium. Medium severity is determined by the organization that published the compliance items.
ssMediumCount :: Lens' SeveritySummary (Maybe Int)
ssMediumCount = lens _ssMediumCount (\s a -> s {_ssMediumCount = a})

-- | The total number of resources or compliance items that have a severity level of informational. Informational severity is determined by the organization that published the compliance items.
ssInformationalCount :: Lens' SeveritySummary (Maybe Int)
ssInformationalCount = lens _ssInformationalCount (\s a -> s {_ssInformationalCount = a})

-- | The total number of resources or compliance items that have a severity level of critical. Critical severity is determined by the organization that published the compliance items.
ssCriticalCount :: Lens' SeveritySummary (Maybe Int)
ssCriticalCount = lens _ssCriticalCount (\s a -> s {_ssCriticalCount = a})

instance FromJSON SeveritySummary where
  parseJSON =
    withObject
      "SeveritySummary"
      ( \x ->
          SeveritySummary'
            <$> (x .:? "LowCount")
            <*> (x .:? "UnspecifiedCount")
            <*> (x .:? "HighCount")
            <*> (x .:? "MediumCount")
            <*> (x .:? "InformationalCount")
            <*> (x .:? "CriticalCount")
      )

instance Hashable SeveritySummary

instance NFData SeveritySummary
