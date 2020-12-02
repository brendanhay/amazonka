{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.Types.InspectorServiceAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Inspector.Types.InspectorServiceAttributes where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | This data type is used in the 'Finding' data type.
--
--
--
-- /See:/ 'inspectorServiceAttributes' smart constructor.
data InspectorServiceAttributes = InspectorServiceAttributes'
  { _isaRulesPackageARN ::
      !(Maybe Text),
    _isaAssessmentRunARN :: !(Maybe Text),
    _isaSchemaVersion :: !Nat
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InspectorServiceAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'isaRulesPackageARN' - The ARN of the rules package that is used to generate the finding.
--
-- * 'isaAssessmentRunARN' - The ARN of the assessment run during which the finding is generated.
--
-- * 'isaSchemaVersion' - The schema version of this data type.
inspectorServiceAttributes ::
  -- | 'isaSchemaVersion'
  Natural ->
  InspectorServiceAttributes
inspectorServiceAttributes pSchemaVersion_ =
  InspectorServiceAttributes'
    { _isaRulesPackageARN = Nothing,
      _isaAssessmentRunARN = Nothing,
      _isaSchemaVersion = _Nat # pSchemaVersion_
    }

-- | The ARN of the rules package that is used to generate the finding.
isaRulesPackageARN :: Lens' InspectorServiceAttributes (Maybe Text)
isaRulesPackageARN = lens _isaRulesPackageARN (\s a -> s {_isaRulesPackageARN = a})

-- | The ARN of the assessment run during which the finding is generated.
isaAssessmentRunARN :: Lens' InspectorServiceAttributes (Maybe Text)
isaAssessmentRunARN = lens _isaAssessmentRunARN (\s a -> s {_isaAssessmentRunARN = a})

-- | The schema version of this data type.
isaSchemaVersion :: Lens' InspectorServiceAttributes Natural
isaSchemaVersion = lens _isaSchemaVersion (\s a -> s {_isaSchemaVersion = a}) . _Nat

instance FromJSON InspectorServiceAttributes where
  parseJSON =
    withObject
      "InspectorServiceAttributes"
      ( \x ->
          InspectorServiceAttributes'
            <$> (x .:? "rulesPackageArn")
            <*> (x .:? "assessmentRunArn")
            <*> (x .: "schemaVersion")
      )

instance Hashable InspectorServiceAttributes

instance NFData InspectorServiceAttributes
