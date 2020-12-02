{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.PartOfSpeechTag
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.PartOfSpeechTag where

import Network.AWS.Comprehend.Types.PartOfSpeechTagType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Identifies the part of speech represented by the token and gives the confidence that Amazon Comprehend has that the part of speech was correctly identified. For more information about the parts of speech that Amazon Comprehend can identify, see 'how-syntax' .
--
--
--
-- /See:/ 'partOfSpeechTag' smart constructor.
data PartOfSpeechTag = PartOfSpeechTag'
  { _postTag ::
      !(Maybe PartOfSpeechTagType),
    _postScore :: !(Maybe Double)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PartOfSpeechTag' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'postTag' - Identifies the part of speech that the token represents.
--
-- * 'postScore' - The confidence that Amazon Comprehend has that the part of speech was correctly identified.
partOfSpeechTag ::
  PartOfSpeechTag
partOfSpeechTag =
  PartOfSpeechTag' {_postTag = Nothing, _postScore = Nothing}

-- | Identifies the part of speech that the token represents.
postTag :: Lens' PartOfSpeechTag (Maybe PartOfSpeechTagType)
postTag = lens _postTag (\s a -> s {_postTag = a})

-- | The confidence that Amazon Comprehend has that the part of speech was correctly identified.
postScore :: Lens' PartOfSpeechTag (Maybe Double)
postScore = lens _postScore (\s a -> s {_postScore = a})

instance FromJSON PartOfSpeechTag where
  parseJSON =
    withObject
      "PartOfSpeechTag"
      (\x -> PartOfSpeechTag' <$> (x .:? "Tag") <*> (x .:? "Score"))

instance Hashable PartOfSpeechTag

instance NFData PartOfSpeechTag
