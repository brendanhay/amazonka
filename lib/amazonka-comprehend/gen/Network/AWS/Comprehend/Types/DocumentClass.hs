{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.DocumentClass
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.DocumentClass where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Specifies the class that categorizes the document being analyzed
--
--
--
-- /See:/ 'documentClass' smart constructor.
data DocumentClass = DocumentClass'
  { _dcScore :: !(Maybe Double),
    _dcName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DocumentClass' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcScore' - The confidence score that Amazon Comprehend has this class correctly attributed.
--
-- * 'dcName' - The name of the class.
documentClass ::
  DocumentClass
documentClass =
  DocumentClass' {_dcScore = Nothing, _dcName = Nothing}

-- | The confidence score that Amazon Comprehend has this class correctly attributed.
dcScore :: Lens' DocumentClass (Maybe Double)
dcScore = lens _dcScore (\s a -> s {_dcScore = a})

-- | The name of the class.
dcName :: Lens' DocumentClass (Maybe Text)
dcName = lens _dcName (\s a -> s {_dcName = a})

instance FromJSON DocumentClass where
  parseJSON =
    withObject
      "DocumentClass"
      (\x -> DocumentClass' <$> (x .:? "Score") <*> (x .:? "Name"))

instance Hashable DocumentClass

instance NFData DocumentClass
