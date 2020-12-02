{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.Types.TimestampRange
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Inspector.Types.TimestampRange where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | This data type is used in the 'AssessmentRunFilter' data type.
--
--
--
-- /See:/ 'timestampRange' smart constructor.
data TimestampRange = TimestampRange'
  { _trEndDate :: !(Maybe POSIX),
    _trBeginDate :: !(Maybe POSIX)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TimestampRange' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'trEndDate' - The maximum value of the timestamp range.
--
-- * 'trBeginDate' - The minimum value of the timestamp range.
timestampRange ::
  TimestampRange
timestampRange =
  TimestampRange' {_trEndDate = Nothing, _trBeginDate = Nothing}

-- | The maximum value of the timestamp range.
trEndDate :: Lens' TimestampRange (Maybe UTCTime)
trEndDate = lens _trEndDate (\s a -> s {_trEndDate = a}) . mapping _Time

-- | The minimum value of the timestamp range.
trBeginDate :: Lens' TimestampRange (Maybe UTCTime)
trBeginDate = lens _trBeginDate (\s a -> s {_trBeginDate = a}) . mapping _Time

instance Hashable TimestampRange

instance NFData TimestampRange

instance ToJSON TimestampRange where
  toJSON TimestampRange' {..} =
    object
      ( catMaybes
          [("endDate" .=) <$> _trEndDate, ("beginDate" .=) <$> _trBeginDate]
      )
