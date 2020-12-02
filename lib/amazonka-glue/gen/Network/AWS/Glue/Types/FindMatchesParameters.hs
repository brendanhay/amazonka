{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.FindMatchesParameters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.FindMatchesParameters where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The parameters to configure the find matches transform.
--
--
--
-- /See:/ 'findMatchesParameters' smart constructor.
data FindMatchesParameters = FindMatchesParameters'
  { _fmpEnforceProvidedLabels ::
      !(Maybe Bool),
    _fmpAccuracyCostTradeoff :: !(Maybe Double),
    _fmpPrecisionRecallTradeoff :: !(Maybe Double),
    _fmpPrimaryKeyColumnName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'FindMatchesParameters' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fmpEnforceProvidedLabels' - The value to switch on or off to force the output to match the provided labels from users. If the value is @True@ , the @find matches@ transform forces the output to match the provided labels. The results override the normal conflation results. If the value is @False@ , the @find matches@ transform does not ensure all the labels provided are respected, and the results rely on the trained model. Note that setting this value to true may increase the conflation execution time.
--
-- * 'fmpAccuracyCostTradeoff' - The value that is selected when tuning your transform for a balance between accuracy and cost. A value of 0.5 means that the system balances accuracy and cost concerns. A value of 1.0 means a bias purely for accuracy, which typically results in a higher cost, sometimes substantially higher. A value of 0.0 means a bias purely for cost, which results in a less accurate @FindMatches@ transform, sometimes with unacceptable accuracy. Accuracy measures how well the transform finds true positives and true negatives. Increasing accuracy requires more machine resources and cost. But it also results in increased recall.  Cost measures how many compute resources, and thus money, are consumed to run the transform.
--
-- * 'fmpPrecisionRecallTradeoff' - The value selected when tuning your transform for a balance between precision and recall. A value of 0.5 means no preference; a value of 1.0 means a bias purely for precision, and a value of 0.0 means a bias for recall. Because this is a tradeoff, choosing values close to 1.0 means very low recall, and choosing values close to 0.0 results in very low precision. The precision metric indicates how often your model is correct when it predicts a match.  The recall metric indicates that for an actual match, how often your model predicts the match.
--
-- * 'fmpPrimaryKeyColumnName' - The name of a column that uniquely identifies rows in the source table. Used to help identify matching records.
findMatchesParameters ::
  FindMatchesParameters
findMatchesParameters =
  FindMatchesParameters'
    { _fmpEnforceProvidedLabels = Nothing,
      _fmpAccuracyCostTradeoff = Nothing,
      _fmpPrecisionRecallTradeoff = Nothing,
      _fmpPrimaryKeyColumnName = Nothing
    }

-- | The value to switch on or off to force the output to match the provided labels from users. If the value is @True@ , the @find matches@ transform forces the output to match the provided labels. The results override the normal conflation results. If the value is @False@ , the @find matches@ transform does not ensure all the labels provided are respected, and the results rely on the trained model. Note that setting this value to true may increase the conflation execution time.
fmpEnforceProvidedLabels :: Lens' FindMatchesParameters (Maybe Bool)
fmpEnforceProvidedLabels = lens _fmpEnforceProvidedLabels (\s a -> s {_fmpEnforceProvidedLabels = a})

-- | The value that is selected when tuning your transform for a balance between accuracy and cost. A value of 0.5 means that the system balances accuracy and cost concerns. A value of 1.0 means a bias purely for accuracy, which typically results in a higher cost, sometimes substantially higher. A value of 0.0 means a bias purely for cost, which results in a less accurate @FindMatches@ transform, sometimes with unacceptable accuracy. Accuracy measures how well the transform finds true positives and true negatives. Increasing accuracy requires more machine resources and cost. But it also results in increased recall.  Cost measures how many compute resources, and thus money, are consumed to run the transform.
fmpAccuracyCostTradeoff :: Lens' FindMatchesParameters (Maybe Double)
fmpAccuracyCostTradeoff = lens _fmpAccuracyCostTradeoff (\s a -> s {_fmpAccuracyCostTradeoff = a})

-- | The value selected when tuning your transform for a balance between precision and recall. A value of 0.5 means no preference; a value of 1.0 means a bias purely for precision, and a value of 0.0 means a bias for recall. Because this is a tradeoff, choosing values close to 1.0 means very low recall, and choosing values close to 0.0 results in very low precision. The precision metric indicates how often your model is correct when it predicts a match.  The recall metric indicates that for an actual match, how often your model predicts the match.
fmpPrecisionRecallTradeoff :: Lens' FindMatchesParameters (Maybe Double)
fmpPrecisionRecallTradeoff = lens _fmpPrecisionRecallTradeoff (\s a -> s {_fmpPrecisionRecallTradeoff = a})

-- | The name of a column that uniquely identifies rows in the source table. Used to help identify matching records.
fmpPrimaryKeyColumnName :: Lens' FindMatchesParameters (Maybe Text)
fmpPrimaryKeyColumnName = lens _fmpPrimaryKeyColumnName (\s a -> s {_fmpPrimaryKeyColumnName = a})

instance FromJSON FindMatchesParameters where
  parseJSON =
    withObject
      "FindMatchesParameters"
      ( \x ->
          FindMatchesParameters'
            <$> (x .:? "EnforceProvidedLabels")
            <*> (x .:? "AccuracyCostTradeoff")
            <*> (x .:? "PrecisionRecallTradeoff")
            <*> (x .:? "PrimaryKeyColumnName")
      )

instance Hashable FindMatchesParameters

instance NFData FindMatchesParameters

instance ToJSON FindMatchesParameters where
  toJSON FindMatchesParameters' {..} =
    object
      ( catMaybes
          [ ("EnforceProvidedLabels" .=) <$> _fmpEnforceProvidedLabels,
            ("AccuracyCostTradeoff" .=) <$> _fmpAccuracyCostTradeoff,
            ("PrecisionRecallTradeoff" .=) <$> _fmpPrecisionRecallTradeoff,
            ("PrimaryKeyColumnName" .=) <$> _fmpPrimaryKeyColumnName
          ]
      )
