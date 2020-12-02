{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.BatchDetectSyntaxItemResult
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.BatchDetectSyntaxItemResult where

import Network.AWS.Comprehend.Types.SyntaxToken
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The result of calling the operation. The operation returns one object that is successfully processed by the operation.
--
--
--
-- /See:/ 'batchDetectSyntaxItemResult' smart constructor.
data BatchDetectSyntaxItemResult = BatchDetectSyntaxItemResult'
  { _bdsirIndex ::
      !(Maybe Int),
    _bdsirSyntaxTokens ::
      !(Maybe [SyntaxToken])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BatchDetectSyntaxItemResult' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bdsirIndex' - The zero-based index of the document in the input list.
--
-- * 'bdsirSyntaxTokens' - The syntax tokens for the words in the document, one token for each word.
batchDetectSyntaxItemResult ::
  BatchDetectSyntaxItemResult
batchDetectSyntaxItemResult =
  BatchDetectSyntaxItemResult'
    { _bdsirIndex = Nothing,
      _bdsirSyntaxTokens = Nothing
    }

-- | The zero-based index of the document in the input list.
bdsirIndex :: Lens' BatchDetectSyntaxItemResult (Maybe Int)
bdsirIndex = lens _bdsirIndex (\s a -> s {_bdsirIndex = a})

-- | The syntax tokens for the words in the document, one token for each word.
bdsirSyntaxTokens :: Lens' BatchDetectSyntaxItemResult [SyntaxToken]
bdsirSyntaxTokens = lens _bdsirSyntaxTokens (\s a -> s {_bdsirSyntaxTokens = a}) . _Default . _Coerce

instance FromJSON BatchDetectSyntaxItemResult where
  parseJSON =
    withObject
      "BatchDetectSyntaxItemResult"
      ( \x ->
          BatchDetectSyntaxItemResult'
            <$> (x .:? "Index") <*> (x .:? "SyntaxTokens" .!= mempty)
      )

instance Hashable BatchDetectSyntaxItemResult

instance NFData BatchDetectSyntaxItemResult
