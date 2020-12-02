{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.DocumentClassifierFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.DocumentClassifierFilter where

import Network.AWS.Comprehend.Types.ModelStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides information for filtering a list of document classifiers. You can only specify one filtering parameter in a request. For more information, see the operation.
--
--
--
-- /See:/ 'documentClassifierFilter' smart constructor.
data DocumentClassifierFilter = DocumentClassifierFilter'
  { _dcfStatus ::
      !(Maybe ModelStatus),
    _dcfSubmitTimeAfter :: !(Maybe POSIX),
    _dcfSubmitTimeBefore :: !(Maybe POSIX)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DocumentClassifierFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcfStatus' - Filters the list of classifiers based on status.
--
-- * 'dcfSubmitTimeAfter' - Filters the list of classifiers based on the time that the classifier was submitted for processing. Returns only classifiers submitted after the specified time. Classifiers are returned in descending order, newest to oldest.
--
-- * 'dcfSubmitTimeBefore' - Filters the list of classifiers based on the time that the classifier was submitted for processing. Returns only classifiers submitted before the specified time. Classifiers are returned in ascending order, oldest to newest.
documentClassifierFilter ::
  DocumentClassifierFilter
documentClassifierFilter =
  DocumentClassifierFilter'
    { _dcfStatus = Nothing,
      _dcfSubmitTimeAfter = Nothing,
      _dcfSubmitTimeBefore = Nothing
    }

-- | Filters the list of classifiers based on status.
dcfStatus :: Lens' DocumentClassifierFilter (Maybe ModelStatus)
dcfStatus = lens _dcfStatus (\s a -> s {_dcfStatus = a})

-- | Filters the list of classifiers based on the time that the classifier was submitted for processing. Returns only classifiers submitted after the specified time. Classifiers are returned in descending order, newest to oldest.
dcfSubmitTimeAfter :: Lens' DocumentClassifierFilter (Maybe UTCTime)
dcfSubmitTimeAfter = lens _dcfSubmitTimeAfter (\s a -> s {_dcfSubmitTimeAfter = a}) . mapping _Time

-- | Filters the list of classifiers based on the time that the classifier was submitted for processing. Returns only classifiers submitted before the specified time. Classifiers are returned in ascending order, oldest to newest.
dcfSubmitTimeBefore :: Lens' DocumentClassifierFilter (Maybe UTCTime)
dcfSubmitTimeBefore = lens _dcfSubmitTimeBefore (\s a -> s {_dcfSubmitTimeBefore = a}) . mapping _Time

instance Hashable DocumentClassifierFilter

instance NFData DocumentClassifierFilter

instance ToJSON DocumentClassifierFilter where
  toJSON DocumentClassifierFilter' {..} =
    object
      ( catMaybes
          [ ("Status" .=) <$> _dcfStatus,
            ("SubmitTimeAfter" .=) <$> _dcfSubmitTimeAfter,
            ("SubmitTimeBefore" .=) <$> _dcfSubmitTimeBefore
          ]
      )
