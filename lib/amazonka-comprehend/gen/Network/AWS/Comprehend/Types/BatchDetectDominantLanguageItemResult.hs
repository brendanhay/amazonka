{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.BatchDetectDominantLanguageItemResult
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.BatchDetectDominantLanguageItemResult where

import Network.AWS.Comprehend.Types.DominantLanguage
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The result of calling the operation. The operation returns one object for each document that is successfully processed by the operation.
--
--
--
-- /See:/ 'batchDetectDominantLanguageItemResult' smart constructor.
data BatchDetectDominantLanguageItemResult = BatchDetectDominantLanguageItemResult'
  { _bddlirLanguages ::
      !( Maybe
           [DominantLanguage]
       ),
    _bddlirIndex ::
      !(Maybe Int)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BatchDetectDominantLanguageItemResult' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bddlirLanguages' - One or more 'DominantLanguage' objects describing the dominant languages in the document.
--
-- * 'bddlirIndex' - The zero-based index of the document in the input list.
batchDetectDominantLanguageItemResult ::
  BatchDetectDominantLanguageItemResult
batchDetectDominantLanguageItemResult =
  BatchDetectDominantLanguageItemResult'
    { _bddlirLanguages =
        Nothing,
      _bddlirIndex = Nothing
    }

-- | One or more 'DominantLanguage' objects describing the dominant languages in the document.
bddlirLanguages :: Lens' BatchDetectDominantLanguageItemResult [DominantLanguage]
bddlirLanguages = lens _bddlirLanguages (\s a -> s {_bddlirLanguages = a}) . _Default . _Coerce

-- | The zero-based index of the document in the input list.
bddlirIndex :: Lens' BatchDetectDominantLanguageItemResult (Maybe Int)
bddlirIndex = lens _bddlirIndex (\s a -> s {_bddlirIndex = a})

instance FromJSON BatchDetectDominantLanguageItemResult where
  parseJSON =
    withObject
      "BatchDetectDominantLanguageItemResult"
      ( \x ->
          BatchDetectDominantLanguageItemResult'
            <$> (x .:? "Languages" .!= mempty) <*> (x .:? "Index")
      )

instance Hashable BatchDetectDominantLanguageItemResult

instance NFData BatchDetectDominantLanguageItemResult
