{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.Rule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.Rule where

import Network.AWS.CloudDirectory.Types.RuleType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains an Amazon Resource Name (ARN) and parameters that are associated with the rule.
--
--
--
-- /See:/ 'rule' smart constructor.
data Rule = Rule'
  { _rParameters :: !(Maybe (Map Text (Text))),
    _rType :: !(Maybe RuleType)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Rule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rParameters' - The minimum and maximum parameters that are associated with the rule.
--
-- * 'rType' - The type of attribute validation rule.
rule ::
  Rule
rule = Rule' {_rParameters = Nothing, _rType = Nothing}

-- | The minimum and maximum parameters that are associated with the rule.
rParameters :: Lens' Rule (HashMap Text (Text))
rParameters = lens _rParameters (\s a -> s {_rParameters = a}) . _Default . _Map

-- | The type of attribute validation rule.
rType :: Lens' Rule (Maybe RuleType)
rType = lens _rType (\s a -> s {_rType = a})

instance FromJSON Rule where
  parseJSON =
    withObject
      "Rule"
      ( \x ->
          Rule' <$> (x .:? "Parameters" .!= mempty) <*> (x .:? "Type")
      )

instance Hashable Rule

instance NFData Rule

instance ToJSON Rule where
  toJSON Rule' {..} =
    object
      ( catMaybes
          [("Parameters" .=) <$> _rParameters, ("Type" .=) <$> _rType]
      )
