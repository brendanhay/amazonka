{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.SimulatePolicyResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.SimulatePolicyResponse where

import Network.AWS.IAM.Types.EvaluationResult
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains the response to a successful 'SimulatePrincipalPolicy' or 'SimulateCustomPolicy' request.
--
--
--
-- /See:/ 'simulatePolicyResponse' smart constructor.
data SimulatePolicyResponse = SimulatePolicyResponse'
  { _spEvaluationResults ::
      !(Maybe [EvaluationResult]),
    _spMarker :: !(Maybe Text),
    _spIsTruncated :: !(Maybe Bool)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SimulatePolicyResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'spEvaluationResults' - The results of the simulation.
--
-- * 'spMarker' - When @IsTruncated@ is @true@ , this element is present and contains the value to use for the @Marker@ parameter in a subsequent pagination request.
--
-- * 'spIsTruncated' - A flag that indicates whether there are more items to return. If your results were truncated, you can make a subsequent pagination request using the @Marker@ request parameter to retrieve more items. Note that IAM might return fewer than the @MaxItems@ number of results even when there are more results available. We recommend that you check @IsTruncated@ after every call to ensure that you receive all your results.
simulatePolicyResponse ::
  SimulatePolicyResponse
simulatePolicyResponse =
  SimulatePolicyResponse'
    { _spEvaluationResults = Nothing,
      _spMarker = Nothing,
      _spIsTruncated = Nothing
    }

-- | The results of the simulation.
spEvaluationResults :: Lens' SimulatePolicyResponse [EvaluationResult]
spEvaluationResults = lens _spEvaluationResults (\s a -> s {_spEvaluationResults = a}) . _Default . _Coerce

-- | When @IsTruncated@ is @true@ , this element is present and contains the value to use for the @Marker@ parameter in a subsequent pagination request.
spMarker :: Lens' SimulatePolicyResponse (Maybe Text)
spMarker = lens _spMarker (\s a -> s {_spMarker = a})

-- | A flag that indicates whether there are more items to return. If your results were truncated, you can make a subsequent pagination request using the @Marker@ request parameter to retrieve more items. Note that IAM might return fewer than the @MaxItems@ number of results even when there are more results available. We recommend that you check @IsTruncated@ after every call to ensure that you receive all your results.
spIsTruncated :: Lens' SimulatePolicyResponse (Maybe Bool)
spIsTruncated = lens _spIsTruncated (\s a -> s {_spIsTruncated = a})

instance FromXML SimulatePolicyResponse where
  parseXML x =
    SimulatePolicyResponse'
      <$> ( x .@? "EvaluationResults" .!@ mempty
              >>= may (parseXMLList "member")
          )
      <*> (x .@? "Marker")
      <*> (x .@? "IsTruncated")

instance Hashable SimulatePolicyResponse

instance NFData SimulatePolicyResponse
