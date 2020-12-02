{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.BusinessReportContentRange
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.BusinessReportContentRange where

import Network.AWS.AlexaBusiness.Types.BusinessReportInterval
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The content range of the report.
--
--
--
-- /See:/ 'businessReportContentRange' smart constructor.
newtype BusinessReportContentRange = BusinessReportContentRange'
  { _brcrInterval ::
      BusinessReportInterval
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BusinessReportContentRange' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'brcrInterval' - The interval of the content range.
businessReportContentRange ::
  -- | 'brcrInterval'
  BusinessReportInterval ->
  BusinessReportContentRange
businessReportContentRange pInterval_ =
  BusinessReportContentRange' {_brcrInterval = pInterval_}

-- | The interval of the content range.
brcrInterval :: Lens' BusinessReportContentRange BusinessReportInterval
brcrInterval = lens _brcrInterval (\s a -> s {_brcrInterval = a})

instance FromJSON BusinessReportContentRange where
  parseJSON =
    withObject
      "BusinessReportContentRange"
      (\x -> BusinessReportContentRange' <$> (x .: "Interval"))

instance Hashable BusinessReportContentRange

instance NFData BusinessReportContentRange

instance ToJSON BusinessReportContentRange where
  toJSON BusinessReportContentRange' {..} =
    object (catMaybes [Just ("Interval" .= _brcrInterval)])
