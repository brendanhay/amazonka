{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.Types.AssessmentTemplateFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Inspector.Types.AssessmentTemplateFilter where

import Network.AWS.Inspector.Types.DurationRange
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Used as the request parameter in the 'ListAssessmentTemplates' action.
--
--
--
-- /See:/ 'assessmentTemplateFilter' smart constructor.
data AssessmentTemplateFilter = AssessmentTemplateFilter'
  { _atfNamePattern ::
      !(Maybe Text),
    _atfRulesPackageARNs :: !(Maybe [Text]),
    _atfDurationRange ::
      !(Maybe DurationRange)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AssessmentTemplateFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'atfNamePattern' - For a record to match a filter, an explicit value or a string that contains a wildcard that is specified for this data type property must match the value of the __assessmentTemplateName__ property of the 'AssessmentTemplate' data type.
--
-- * 'atfRulesPackageARNs' - For a record to match a filter, the values that are specified for this data type property must be contained in the list of values of the __rulesPackageArns__ property of the 'AssessmentTemplate' data type.
--
-- * 'atfDurationRange' - For a record to match a filter, the value specified for this data type property must inclusively match any value between the specified minimum and maximum values of the __durationInSeconds__ property of the 'AssessmentTemplate' data type.
assessmentTemplateFilter ::
  AssessmentTemplateFilter
assessmentTemplateFilter =
  AssessmentTemplateFilter'
    { _atfNamePattern = Nothing,
      _atfRulesPackageARNs = Nothing,
      _atfDurationRange = Nothing
    }

-- | For a record to match a filter, an explicit value or a string that contains a wildcard that is specified for this data type property must match the value of the __assessmentTemplateName__ property of the 'AssessmentTemplate' data type.
atfNamePattern :: Lens' AssessmentTemplateFilter (Maybe Text)
atfNamePattern = lens _atfNamePattern (\s a -> s {_atfNamePattern = a})

-- | For a record to match a filter, the values that are specified for this data type property must be contained in the list of values of the __rulesPackageArns__ property of the 'AssessmentTemplate' data type.
atfRulesPackageARNs :: Lens' AssessmentTemplateFilter [Text]
atfRulesPackageARNs = lens _atfRulesPackageARNs (\s a -> s {_atfRulesPackageARNs = a}) . _Default . _Coerce

-- | For a record to match a filter, the value specified for this data type property must inclusively match any value between the specified minimum and maximum values of the __durationInSeconds__ property of the 'AssessmentTemplate' data type.
atfDurationRange :: Lens' AssessmentTemplateFilter (Maybe DurationRange)
atfDurationRange = lens _atfDurationRange (\s a -> s {_atfDurationRange = a})

instance Hashable AssessmentTemplateFilter

instance NFData AssessmentTemplateFilter

instance ToJSON AssessmentTemplateFilter where
  toJSON AssessmentTemplateFilter' {..} =
    object
      ( catMaybes
          [ ("namePattern" .=) <$> _atfNamePattern,
            ("rulesPackageArns" .=) <$> _atfRulesPackageARNs,
            ("durationRange" .=) <$> _atfDurationRange
          ]
      )
