{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.PromptSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.PromptSummary where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information about the prompt.
--
--
--
-- /See:/ 'promptSummary' smart constructor.
data PromptSummary = PromptSummary'
  { _psARN :: !(Maybe Text),
    _psName :: !(Maybe Text),
    _psId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PromptSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'psARN' - The Amazon Resource Name (ARN) of the prompt.
--
-- * 'psName' - The name of the prompt.
--
-- * 'psId' - The identifier of the prompt.
promptSummary ::
  PromptSummary
promptSummary =
  PromptSummary'
    { _psARN = Nothing,
      _psName = Nothing,
      _psId = Nothing
    }

-- | The Amazon Resource Name (ARN) of the prompt.
psARN :: Lens' PromptSummary (Maybe Text)
psARN = lens _psARN (\s a -> s {_psARN = a})

-- | The name of the prompt.
psName :: Lens' PromptSummary (Maybe Text)
psName = lens _psName (\s a -> s {_psName = a})

-- | The identifier of the prompt.
psId :: Lens' PromptSummary (Maybe Text)
psId = lens _psId (\s a -> s {_psId = a})

instance FromJSON PromptSummary where
  parseJSON =
    withObject
      "PromptSummary"
      ( \x ->
          PromptSummary'
            <$> (x .:? "Arn") <*> (x .:? "Name") <*> (x .:? "Id")
      )

instance Hashable PromptSummary

instance NFData PromptSummary
