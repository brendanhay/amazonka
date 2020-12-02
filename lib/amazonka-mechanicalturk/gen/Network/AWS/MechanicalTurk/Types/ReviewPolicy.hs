{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.Types.ReviewPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MechanicalTurk.Types.ReviewPolicy where

import Network.AWS.Lens
import Network.AWS.MechanicalTurk.Types.PolicyParameter
import Network.AWS.Prelude

-- | HIT Review Policy data structures represent HIT review policies, which you specify when you create a HIT.
--
--
--
-- /See:/ 'reviewPolicy' smart constructor.
data ReviewPolicy = ReviewPolicy'
  { _rpParameters ::
      !(Maybe [PolicyParameter]),
    _rpPolicyName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ReviewPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rpParameters' - Name of the parameter from the Review policy.
--
-- * 'rpPolicyName' - Name of a Review Policy: SimplePlurality/2011-09-01 or ScoreMyKnownAnswers/2011-09-01
reviewPolicy ::
  -- | 'rpPolicyName'
  Text ->
  ReviewPolicy
reviewPolicy pPolicyName_ =
  ReviewPolicy'
    { _rpParameters = Nothing,
      _rpPolicyName = pPolicyName_
    }

-- | Name of the parameter from the Review policy.
rpParameters :: Lens' ReviewPolicy [PolicyParameter]
rpParameters = lens _rpParameters (\s a -> s {_rpParameters = a}) . _Default . _Coerce

-- | Name of a Review Policy: SimplePlurality/2011-09-01 or ScoreMyKnownAnswers/2011-09-01
rpPolicyName :: Lens' ReviewPolicy Text
rpPolicyName = lens _rpPolicyName (\s a -> s {_rpPolicyName = a})

instance FromJSON ReviewPolicy where
  parseJSON =
    withObject
      "ReviewPolicy"
      ( \x ->
          ReviewPolicy'
            <$> (x .:? "Parameters" .!= mempty) <*> (x .: "PolicyName")
      )

instance Hashable ReviewPolicy

instance NFData ReviewPolicy

instance ToJSON ReviewPolicy where
  toJSON ReviewPolicy' {..} =
    object
      ( catMaybes
          [ ("Parameters" .=) <$> _rpParameters,
            Just ("PolicyName" .= _rpPolicyName)
          ]
      )
