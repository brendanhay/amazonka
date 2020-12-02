{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.Types.AssessmentTargetFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Inspector.Types.AssessmentTargetFilter where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Used as the request parameter in the 'ListAssessmentTargets' action.
--
--
--
-- /See:/ 'assessmentTargetFilter' smart constructor.
newtype AssessmentTargetFilter = AssessmentTargetFilter'
  { _atfAssessmentTargetNamePattern ::
      Maybe Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AssessmentTargetFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'atfAssessmentTargetNamePattern' - For a record to match a filter, an explicit value or a string that contains a wildcard that is specified for this data type property must match the value of the __assessmentTargetName__ property of the 'AssessmentTarget' data type.
assessmentTargetFilter ::
  AssessmentTargetFilter
assessmentTargetFilter =
  AssessmentTargetFilter'
    { _atfAssessmentTargetNamePattern =
        Nothing
    }

-- | For a record to match a filter, an explicit value or a string that contains a wildcard that is specified for this data type property must match the value of the __assessmentTargetName__ property of the 'AssessmentTarget' data type.
atfAssessmentTargetNamePattern :: Lens' AssessmentTargetFilter (Maybe Text)
atfAssessmentTargetNamePattern = lens _atfAssessmentTargetNamePattern (\s a -> s {_atfAssessmentTargetNamePattern = a})

instance Hashable AssessmentTargetFilter

instance NFData AssessmentTargetFilter

instance ToJSON AssessmentTargetFilter where
  toJSON AssessmentTargetFilter' {..} =
    object
      ( catMaybes
          [ ("assessmentTargetNamePattern" .=)
              <$> _atfAssessmentTargetNamePattern
          ]
      )
