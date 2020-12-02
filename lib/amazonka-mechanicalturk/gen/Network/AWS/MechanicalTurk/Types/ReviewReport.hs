{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.Types.ReviewReport
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MechanicalTurk.Types.ReviewReport where

import Network.AWS.Lens
import Network.AWS.MechanicalTurk.Types.ReviewActionDetail
import Network.AWS.MechanicalTurk.Types.ReviewResultDetail
import Network.AWS.Prelude

-- | Contains both ReviewResult and ReviewAction elements for a particular HIT.
--
--
--
-- /See:/ 'reviewReport' smart constructor.
data ReviewReport = ReviewReport'
  { _rrReviewActions ::
      !(Maybe [ReviewActionDetail]),
    _rrReviewResults :: !(Maybe [ReviewResultDetail])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ReviewReport' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rrReviewActions' - A list of ReviewAction objects for each action specified in the Review Policy.
--
-- * 'rrReviewResults' - A list of ReviewResults objects for each action specified in the Review Policy.
reviewReport ::
  ReviewReport
reviewReport =
  ReviewReport'
    { _rrReviewActions = Nothing,
      _rrReviewResults = Nothing
    }

-- | A list of ReviewAction objects for each action specified in the Review Policy.
rrReviewActions :: Lens' ReviewReport [ReviewActionDetail]
rrReviewActions = lens _rrReviewActions (\s a -> s {_rrReviewActions = a}) . _Default . _Coerce

-- | A list of ReviewResults objects for each action specified in the Review Policy.
rrReviewResults :: Lens' ReviewReport [ReviewResultDetail]
rrReviewResults = lens _rrReviewResults (\s a -> s {_rrReviewResults = a}) . _Default . _Coerce

instance FromJSON ReviewReport where
  parseJSON =
    withObject
      "ReviewReport"
      ( \x ->
          ReviewReport'
            <$> (x .:? "ReviewActions" .!= mempty)
            <*> (x .:? "ReviewResults" .!= mempty)
      )

instance Hashable ReviewReport

instance NFData ReviewReport
