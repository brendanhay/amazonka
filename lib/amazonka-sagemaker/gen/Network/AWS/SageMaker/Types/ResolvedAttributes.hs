{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ResolvedAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ResolvedAttributes where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SageMaker.Types.AutoMLJobCompletionCriteria
import Network.AWS.SageMaker.Types.AutoMLJobObjective
import Network.AWS.SageMaker.Types.ProblemType

-- | The resolved attributes.
--
--
--
-- /See:/ 'resolvedAttributes' smart constructor.
data ResolvedAttributes = ResolvedAttributes'
  { _raProblemType ::
      !(Maybe ProblemType),
    _raAutoMLJobObjective :: !(Maybe AutoMLJobObjective),
    _raCompletionCriteria ::
      !(Maybe AutoMLJobCompletionCriteria)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ResolvedAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'raProblemType' - The problem type.
--
-- * 'raAutoMLJobObjective' - Undocumented member.
--
-- * 'raCompletionCriteria' - Undocumented member.
resolvedAttributes ::
  ResolvedAttributes
resolvedAttributes =
  ResolvedAttributes'
    { _raProblemType = Nothing,
      _raAutoMLJobObjective = Nothing,
      _raCompletionCriteria = Nothing
    }

-- | The problem type.
raProblemType :: Lens' ResolvedAttributes (Maybe ProblemType)
raProblemType = lens _raProblemType (\s a -> s {_raProblemType = a})

-- | Undocumented member.
raAutoMLJobObjective :: Lens' ResolvedAttributes (Maybe AutoMLJobObjective)
raAutoMLJobObjective = lens _raAutoMLJobObjective (\s a -> s {_raAutoMLJobObjective = a})

-- | Undocumented member.
raCompletionCriteria :: Lens' ResolvedAttributes (Maybe AutoMLJobCompletionCriteria)
raCompletionCriteria = lens _raCompletionCriteria (\s a -> s {_raCompletionCriteria = a})

instance FromJSON ResolvedAttributes where
  parseJSON =
    withObject
      "ResolvedAttributes"
      ( \x ->
          ResolvedAttributes'
            <$> (x .:? "ProblemType")
            <*> (x .:? "AutoMLJobObjective")
            <*> (x .:? "CompletionCriteria")
      )

instance Hashable ResolvedAttributes

instance NFData ResolvedAttributes
