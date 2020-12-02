{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.AnalyticsFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.AnalyticsFilter where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.AnalyticsAndOperator
import Network.AWS.S3.Types.Tag

-- | The filter used to describe a set of objects for analyses. A filter must have exactly one prefix, one tag, or one conjunction (AnalyticsAndOperator). If no filter is provided, all objects will be considered in any analysis.
--
--
--
-- /See:/ 'analyticsFilter' smart constructor.
data AnalyticsFilter = AnalyticsFilter'
  { _afTag :: !(Maybe Tag),
    _afPrefix :: !(Maybe Text),
    _afAnd :: !(Maybe AnalyticsAndOperator)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AnalyticsFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'afTag' - The tag to use when evaluating an analytics filter.
--
-- * 'afPrefix' - The prefix to use when evaluating an analytics filter.
--
-- * 'afAnd' - A conjunction (logical AND) of predicates, which is used in evaluating an analytics filter. The operator must have at least two predicates.
analyticsFilter ::
  AnalyticsFilter
analyticsFilter =
  AnalyticsFilter'
    { _afTag = Nothing,
      _afPrefix = Nothing,
      _afAnd = Nothing
    }

-- | The tag to use when evaluating an analytics filter.
afTag :: Lens' AnalyticsFilter (Maybe Tag)
afTag = lens _afTag (\s a -> s {_afTag = a})

-- | The prefix to use when evaluating an analytics filter.
afPrefix :: Lens' AnalyticsFilter (Maybe Text)
afPrefix = lens _afPrefix (\s a -> s {_afPrefix = a})

-- | A conjunction (logical AND) of predicates, which is used in evaluating an analytics filter. The operator must have at least two predicates.
afAnd :: Lens' AnalyticsFilter (Maybe AnalyticsAndOperator)
afAnd = lens _afAnd (\s a -> s {_afAnd = a})

instance FromXML AnalyticsFilter where
  parseXML x =
    AnalyticsFilter'
      <$> (x .@? "Tag") <*> (x .@? "Prefix") <*> (x .@? "And")

instance Hashable AnalyticsFilter

instance NFData AnalyticsFilter

instance ToXML AnalyticsFilter where
  toXML AnalyticsFilter' {..} =
    mconcat ["Tag" @= _afTag, "Prefix" @= _afPrefix, "And" @= _afAnd]
