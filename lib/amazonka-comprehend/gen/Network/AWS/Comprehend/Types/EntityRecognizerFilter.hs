{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.EntityRecognizerFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.EntityRecognizerFilter where

import Network.AWS.Comprehend.Types.ModelStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides information for filtering a list of entity recognizers. You can only specify one filtering parameter in a request. For more information, see the operation./>
--
--
--
-- /See:/ 'entityRecognizerFilter' smart constructor.
data EntityRecognizerFilter = EntityRecognizerFilter'
  { _erfStatus ::
      !(Maybe ModelStatus),
    _erfSubmitTimeAfter :: !(Maybe POSIX),
    _erfSubmitTimeBefore :: !(Maybe POSIX)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EntityRecognizerFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'erfStatus' - The status of an entity recognizer.
--
-- * 'erfSubmitTimeAfter' - Filters the list of entities based on the time that the list was submitted for processing. Returns only jobs submitted after the specified time. Jobs are returned in ascending order, oldest to newest.
--
-- * 'erfSubmitTimeBefore' - Filters the list of entities based on the time that the list was submitted for processing. Returns only jobs submitted before the specified time. Jobs are returned in descending order, newest to oldest.
entityRecognizerFilter ::
  EntityRecognizerFilter
entityRecognizerFilter =
  EntityRecognizerFilter'
    { _erfStatus = Nothing,
      _erfSubmitTimeAfter = Nothing,
      _erfSubmitTimeBefore = Nothing
    }

-- | The status of an entity recognizer.
erfStatus :: Lens' EntityRecognizerFilter (Maybe ModelStatus)
erfStatus = lens _erfStatus (\s a -> s {_erfStatus = a})

-- | Filters the list of entities based on the time that the list was submitted for processing. Returns only jobs submitted after the specified time. Jobs are returned in ascending order, oldest to newest.
erfSubmitTimeAfter :: Lens' EntityRecognizerFilter (Maybe UTCTime)
erfSubmitTimeAfter = lens _erfSubmitTimeAfter (\s a -> s {_erfSubmitTimeAfter = a}) . mapping _Time

-- | Filters the list of entities based on the time that the list was submitted for processing. Returns only jobs submitted before the specified time. Jobs are returned in descending order, newest to oldest.
erfSubmitTimeBefore :: Lens' EntityRecognizerFilter (Maybe UTCTime)
erfSubmitTimeBefore = lens _erfSubmitTimeBefore (\s a -> s {_erfSubmitTimeBefore = a}) . mapping _Time

instance Hashable EntityRecognizerFilter

instance NFData EntityRecognizerFilter

instance ToJSON EntityRecognizerFilter where
  toJSON EntityRecognizerFilter' {..} =
    object
      ( catMaybes
          [ ("Status" .=) <$> _erfStatus,
            ("SubmitTimeAfter" .=) <$> _erfSubmitTimeAfter,
            ("SubmitTimeBefore" .=) <$> _erfSubmitTimeBefore
          ]
      )
