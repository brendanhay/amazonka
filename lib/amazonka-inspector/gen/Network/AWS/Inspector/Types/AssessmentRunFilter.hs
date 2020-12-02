{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.Types.AssessmentRunFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Inspector.Types.AssessmentRunFilter where

import Network.AWS.Inspector.Types.AssessmentRunState
import Network.AWS.Inspector.Types.DurationRange
import Network.AWS.Inspector.Types.TimestampRange
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Used as the request parameter in the 'ListAssessmentRuns' action.
--
--
--
-- /See:/ 'assessmentRunFilter' smart constructor.
data AssessmentRunFilter = AssessmentRunFilter'
  { _arfStates ::
      !(Maybe [AssessmentRunState]),
    _arfNamePattern :: !(Maybe Text),
    _arfStartTimeRange :: !(Maybe TimestampRange),
    _arfStateChangeTimeRange :: !(Maybe TimestampRange),
    _arfRulesPackageARNs :: !(Maybe [Text]),
    _arfCompletionTimeRange :: !(Maybe TimestampRange),
    _arfDurationRange :: !(Maybe DurationRange)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AssessmentRunFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'arfStates' - For a record to match a filter, one of the values specified for this data type property must be the exact match of the value of the __assessmentRunState__ property of the 'AssessmentRun' data type.
--
-- * 'arfNamePattern' - For a record to match a filter, an explicit value or a string containing a wildcard that is specified for this data type property must match the value of the __assessmentRunName__ property of the 'AssessmentRun' data type.
--
-- * 'arfStartTimeRange' - For a record to match a filter, the value that is specified for this data type property must inclusively match any value between the specified minimum and maximum values of the __startTime__ property of the 'AssessmentRun' data type.
--
-- * 'arfStateChangeTimeRange' - For a record to match a filter, the value that is specified for this data type property must match the __stateChangedAt__ property of the 'AssessmentRun' data type.
--
-- * 'arfRulesPackageARNs' - For a record to match a filter, the value that is specified for this data type property must be contained in the list of values of the __rulesPackages__ property of the 'AssessmentRun' data type.
--
-- * 'arfCompletionTimeRange' - For a record to match a filter, the value that is specified for this data type property must inclusively match any value between the specified minimum and maximum values of the __completedAt__ property of the 'AssessmentRun' data type.
--
-- * 'arfDurationRange' - For a record to match a filter, the value that is specified for this data type property must inclusively match any value between the specified minimum and maximum values of the __durationInSeconds__ property of the 'AssessmentRun' data type.
assessmentRunFilter ::
  AssessmentRunFilter
assessmentRunFilter =
  AssessmentRunFilter'
    { _arfStates = Nothing,
      _arfNamePattern = Nothing,
      _arfStartTimeRange = Nothing,
      _arfStateChangeTimeRange = Nothing,
      _arfRulesPackageARNs = Nothing,
      _arfCompletionTimeRange = Nothing,
      _arfDurationRange = Nothing
    }

-- | For a record to match a filter, one of the values specified for this data type property must be the exact match of the value of the __assessmentRunState__ property of the 'AssessmentRun' data type.
arfStates :: Lens' AssessmentRunFilter [AssessmentRunState]
arfStates = lens _arfStates (\s a -> s {_arfStates = a}) . _Default . _Coerce

-- | For a record to match a filter, an explicit value or a string containing a wildcard that is specified for this data type property must match the value of the __assessmentRunName__ property of the 'AssessmentRun' data type.
arfNamePattern :: Lens' AssessmentRunFilter (Maybe Text)
arfNamePattern = lens _arfNamePattern (\s a -> s {_arfNamePattern = a})

-- | For a record to match a filter, the value that is specified for this data type property must inclusively match any value between the specified minimum and maximum values of the __startTime__ property of the 'AssessmentRun' data type.
arfStartTimeRange :: Lens' AssessmentRunFilter (Maybe TimestampRange)
arfStartTimeRange = lens _arfStartTimeRange (\s a -> s {_arfStartTimeRange = a})

-- | For a record to match a filter, the value that is specified for this data type property must match the __stateChangedAt__ property of the 'AssessmentRun' data type.
arfStateChangeTimeRange :: Lens' AssessmentRunFilter (Maybe TimestampRange)
arfStateChangeTimeRange = lens _arfStateChangeTimeRange (\s a -> s {_arfStateChangeTimeRange = a})

-- | For a record to match a filter, the value that is specified for this data type property must be contained in the list of values of the __rulesPackages__ property of the 'AssessmentRun' data type.
arfRulesPackageARNs :: Lens' AssessmentRunFilter [Text]
arfRulesPackageARNs = lens _arfRulesPackageARNs (\s a -> s {_arfRulesPackageARNs = a}) . _Default . _Coerce

-- | For a record to match a filter, the value that is specified for this data type property must inclusively match any value between the specified minimum and maximum values of the __completedAt__ property of the 'AssessmentRun' data type.
arfCompletionTimeRange :: Lens' AssessmentRunFilter (Maybe TimestampRange)
arfCompletionTimeRange = lens _arfCompletionTimeRange (\s a -> s {_arfCompletionTimeRange = a})

-- | For a record to match a filter, the value that is specified for this data type property must inclusively match any value between the specified minimum and maximum values of the __durationInSeconds__ property of the 'AssessmentRun' data type.
arfDurationRange :: Lens' AssessmentRunFilter (Maybe DurationRange)
arfDurationRange = lens _arfDurationRange (\s a -> s {_arfDurationRange = a})

instance Hashable AssessmentRunFilter

instance NFData AssessmentRunFilter

instance ToJSON AssessmentRunFilter where
  toJSON AssessmentRunFilter' {..} =
    object
      ( catMaybes
          [ ("states" .=) <$> _arfStates,
            ("namePattern" .=) <$> _arfNamePattern,
            ("startTimeRange" .=) <$> _arfStartTimeRange,
            ("stateChangeTimeRange" .=) <$> _arfStateChangeTimeRange,
            ("rulesPackageArns" .=) <$> _arfRulesPackageARNs,
            ("completionTimeRange" .=) <$> _arfCompletionTimeRange,
            ("durationRange" .=) <$> _arfDurationRange
          ]
      )
