{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.DocumentLabel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.DocumentLabel where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Specifies one of the label or labels that categorize the document being analyzed.
--
--
--
-- /See:/ 'documentLabel' smart constructor.
data DocumentLabel = DocumentLabel'
  { _dScore :: !(Maybe Double),
    _dName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DocumentLabel' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dScore' - The confidence score that Amazon Comprehend has this label correctly attributed.
--
-- * 'dName' - The name of the label.
documentLabel ::
  DocumentLabel
documentLabel = DocumentLabel' {_dScore = Nothing, _dName = Nothing}

-- | The confidence score that Amazon Comprehend has this label correctly attributed.
dScore :: Lens' DocumentLabel (Maybe Double)
dScore = lens _dScore (\s a -> s {_dScore = a})

-- | The name of the label.
dName :: Lens' DocumentLabel (Maybe Text)
dName = lens _dName (\s a -> s {_dName = a})

instance FromJSON DocumentLabel where
  parseJSON =
    withObject
      "DocumentLabel"
      (\x -> DocumentLabel' <$> (x .:? "Score") <*> (x .:? "Name"))

instance Hashable DocumentLabel

instance NFData DocumentLabel
