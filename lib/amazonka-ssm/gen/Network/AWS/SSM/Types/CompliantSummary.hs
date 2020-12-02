{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.CompliantSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.CompliantSummary where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SSM.Types.SeveritySummary

-- | A summary of resources that are compliant. The summary is organized according to the resource count for each compliance type.
--
--
--
-- /See:/ 'compliantSummary' smart constructor.
data CompliantSummary = CompliantSummary'
  { _csCompliantCount ::
      !(Maybe Int),
    _csSeveritySummary :: !(Maybe SeveritySummary)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CompliantSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csCompliantCount' - The total number of resources that are compliant.
--
-- * 'csSeveritySummary' - A summary of the compliance severity by compliance type.
compliantSummary ::
  CompliantSummary
compliantSummary =
  CompliantSummary'
    { _csCompliantCount = Nothing,
      _csSeveritySummary = Nothing
    }

-- | The total number of resources that are compliant.
csCompliantCount :: Lens' CompliantSummary (Maybe Int)
csCompliantCount = lens _csCompliantCount (\s a -> s {_csCompliantCount = a})

-- | A summary of the compliance severity by compliance type.
csSeveritySummary :: Lens' CompliantSummary (Maybe SeveritySummary)
csSeveritySummary = lens _csSeveritySummary (\s a -> s {_csSeveritySummary = a})

instance FromJSON CompliantSummary where
  parseJSON =
    withObject
      "CompliantSummary"
      ( \x ->
          CompliantSummary'
            <$> (x .:? "CompliantCount") <*> (x .:? "SeveritySummary")
      )

instance Hashable CompliantSummary

instance NFData CompliantSummary
