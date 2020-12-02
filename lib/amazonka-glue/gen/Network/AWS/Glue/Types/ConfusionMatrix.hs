{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.ConfusionMatrix
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.ConfusionMatrix where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The confusion matrix shows you what your transform is predicting accurately and what types of errors it is making.
--
--
-- For more information, see <https://en.wikipedia.org/wiki/Confusion_matrix Confusion matrix> in Wikipedia.
--
--
-- /See:/ 'confusionMatrix' smart constructor.
data ConfusionMatrix = ConfusionMatrix'
  { _cmNumTrueNegatives ::
      !(Maybe Integer),
    _cmNumFalseNegatives :: !(Maybe Integer),
    _cmNumTruePositives :: !(Maybe Integer),
    _cmNumFalsePositives :: !(Maybe Integer)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ConfusionMatrix' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cmNumTrueNegatives' - The number of nonmatches in the data that the transform correctly rejected, in the confusion matrix for your transform.
--
-- * 'cmNumFalseNegatives' - The number of matches in the data that the transform didn't find, in the confusion matrix for your transform.
--
-- * 'cmNumTruePositives' - The number of matches in the data that the transform correctly found, in the confusion matrix for your transform.
--
-- * 'cmNumFalsePositives' - The number of nonmatches in the data that the transform incorrectly classified as a match, in the confusion matrix for your transform.
confusionMatrix ::
  ConfusionMatrix
confusionMatrix =
  ConfusionMatrix'
    { _cmNumTrueNegatives = Nothing,
      _cmNumFalseNegatives = Nothing,
      _cmNumTruePositives = Nothing,
      _cmNumFalsePositives = Nothing
    }

-- | The number of nonmatches in the data that the transform correctly rejected, in the confusion matrix for your transform.
cmNumTrueNegatives :: Lens' ConfusionMatrix (Maybe Integer)
cmNumTrueNegatives = lens _cmNumTrueNegatives (\s a -> s {_cmNumTrueNegatives = a})

-- | The number of matches in the data that the transform didn't find, in the confusion matrix for your transform.
cmNumFalseNegatives :: Lens' ConfusionMatrix (Maybe Integer)
cmNumFalseNegatives = lens _cmNumFalseNegatives (\s a -> s {_cmNumFalseNegatives = a})

-- | The number of matches in the data that the transform correctly found, in the confusion matrix for your transform.
cmNumTruePositives :: Lens' ConfusionMatrix (Maybe Integer)
cmNumTruePositives = lens _cmNumTruePositives (\s a -> s {_cmNumTruePositives = a})

-- | The number of nonmatches in the data that the transform incorrectly classified as a match, in the confusion matrix for your transform.
cmNumFalsePositives :: Lens' ConfusionMatrix (Maybe Integer)
cmNumFalsePositives = lens _cmNumFalsePositives (\s a -> s {_cmNumFalsePositives = a})

instance FromJSON ConfusionMatrix where
  parseJSON =
    withObject
      "ConfusionMatrix"
      ( \x ->
          ConfusionMatrix'
            <$> (x .:? "NumTrueNegatives")
            <*> (x .:? "NumFalseNegatives")
            <*> (x .:? "NumTruePositives")
            <*> (x .:? "NumFalsePositives")
      )

instance Hashable ConfusionMatrix

instance NFData ConfusionMatrix
