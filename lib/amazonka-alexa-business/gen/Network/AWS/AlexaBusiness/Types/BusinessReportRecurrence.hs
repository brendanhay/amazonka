{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.BusinessReportRecurrence
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.BusinessReportRecurrence where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The recurrence of the reports.
--
--
--
-- /See:/ 'businessReportRecurrence' smart constructor.
newtype BusinessReportRecurrence = BusinessReportRecurrence'
  { _brrStartDate ::
      Maybe Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BusinessReportRecurrence' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'brrStartDate' - The start date.
businessReportRecurrence ::
  BusinessReportRecurrence
businessReportRecurrence =
  BusinessReportRecurrence' {_brrStartDate = Nothing}

-- | The start date.
brrStartDate :: Lens' BusinessReportRecurrence (Maybe Text)
brrStartDate = lens _brrStartDate (\s a -> s {_brrStartDate = a})

instance FromJSON BusinessReportRecurrence where
  parseJSON =
    withObject
      "BusinessReportRecurrence"
      (\x -> BusinessReportRecurrence' <$> (x .:? "StartDate"))

instance Hashable BusinessReportRecurrence

instance NFData BusinessReportRecurrence

instance ToJSON BusinessReportRecurrence where
  toJSON BusinessReportRecurrence' {..} =
    object (catMaybes [("StartDate" .=) <$> _brrStartDate])
