{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexRuntime.Types.IntentConfidence
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexRuntime.Types.IntentConfidence where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides a score that indicates the confidence that Amazon Lex has that an intent is the one that satisfies the user's intent.
--
--
--
-- /See:/ 'intentConfidence' smart constructor.
newtype IntentConfidence = IntentConfidence'
  { _icScore ::
      Maybe Double
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'IntentConfidence' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'icScore' - A score that indicates how confident Amazon Lex is that an intent satisfies the user's intent. Ranges between 0.00 and 1.00. Higher scores indicate higher confidence.
intentConfidence ::
  IntentConfidence
intentConfidence = IntentConfidence' {_icScore = Nothing}

-- | A score that indicates how confident Amazon Lex is that an intent satisfies the user's intent. Ranges between 0.00 and 1.00. Higher scores indicate higher confidence.
icScore :: Lens' IntentConfidence (Maybe Double)
icScore = lens _icScore (\s a -> s {_icScore = a})

instance FromJSON IntentConfidence where
  parseJSON =
    withObject
      "IntentConfidence"
      (\x -> IntentConfidence' <$> (x .:? "score"))

instance Hashable IntentConfidence

instance NFData IntentConfidence
