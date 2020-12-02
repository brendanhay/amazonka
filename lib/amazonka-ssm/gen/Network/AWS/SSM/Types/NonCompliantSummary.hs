{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.NonCompliantSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.NonCompliantSummary where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SSM.Types.SeveritySummary

-- | A summary of resources that are not compliant. The summary is organized according to resource type.
--
--
--
-- /See:/ 'nonCompliantSummary' smart constructor.
data NonCompliantSummary = NonCompliantSummary'
  { _ncsNonCompliantCount ::
      !(Maybe Int),
    _ncsSeveritySummary :: !(Maybe SeveritySummary)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'NonCompliantSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ncsNonCompliantCount' - The total number of compliance items that are not compliant.
--
-- * 'ncsSeveritySummary' - A summary of the non-compliance severity by compliance type
nonCompliantSummary ::
  NonCompliantSummary
nonCompliantSummary =
  NonCompliantSummary'
    { _ncsNonCompliantCount = Nothing,
      _ncsSeveritySummary = Nothing
    }

-- | The total number of compliance items that are not compliant.
ncsNonCompliantCount :: Lens' NonCompliantSummary (Maybe Int)
ncsNonCompliantCount = lens _ncsNonCompliantCount (\s a -> s {_ncsNonCompliantCount = a})

-- | A summary of the non-compliance severity by compliance type
ncsSeveritySummary :: Lens' NonCompliantSummary (Maybe SeveritySummary)
ncsSeveritySummary = lens _ncsSeveritySummary (\s a -> s {_ncsSeveritySummary = a})

instance FromJSON NonCompliantSummary where
  parseJSON =
    withObject
      "NonCompliantSummary"
      ( \x ->
          NonCompliantSummary'
            <$> (x .:? "NonCompliantCount") <*> (x .:? "SeveritySummary")
      )

instance Hashable NonCompliantSummary

instance NFData NonCompliantSummary
