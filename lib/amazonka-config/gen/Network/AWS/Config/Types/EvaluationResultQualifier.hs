{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.EvaluationResultQualifier
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.EvaluationResultQualifier where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Identifies an AWS Config rule that evaluated an AWS resource, and provides the type and ID of the resource that the rule evaluated.
--
--
--
-- /See:/ 'evaluationResultQualifier' smart constructor.
data EvaluationResultQualifier = EvaluationResultQualifier'
  { _erqResourceId ::
      !(Maybe Text),
    _erqResourceType :: !(Maybe Text),
    _erqConfigRuleName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EvaluationResultQualifier' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'erqResourceId' - The ID of the evaluated AWS resource.
--
-- * 'erqResourceType' - The type of AWS resource that was evaluated.
--
-- * 'erqConfigRuleName' - The name of the AWS Config rule that was used in the evaluation.
evaluationResultQualifier ::
  EvaluationResultQualifier
evaluationResultQualifier =
  EvaluationResultQualifier'
    { _erqResourceId = Nothing,
      _erqResourceType = Nothing,
      _erqConfigRuleName = Nothing
    }

-- | The ID of the evaluated AWS resource.
erqResourceId :: Lens' EvaluationResultQualifier (Maybe Text)
erqResourceId = lens _erqResourceId (\s a -> s {_erqResourceId = a})

-- | The type of AWS resource that was evaluated.
erqResourceType :: Lens' EvaluationResultQualifier (Maybe Text)
erqResourceType = lens _erqResourceType (\s a -> s {_erqResourceType = a})

-- | The name of the AWS Config rule that was used in the evaluation.
erqConfigRuleName :: Lens' EvaluationResultQualifier (Maybe Text)
erqConfigRuleName = lens _erqConfigRuleName (\s a -> s {_erqConfigRuleName = a})

instance FromJSON EvaluationResultQualifier where
  parseJSON =
    withObject
      "EvaluationResultQualifier"
      ( \x ->
          EvaluationResultQualifier'
            <$> (x .:? "ResourceId")
            <*> (x .:? "ResourceType")
            <*> (x .:? "ConfigRuleName")
      )

instance Hashable EvaluationResultQualifier

instance NFData EvaluationResultQualifier
