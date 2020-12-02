{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.MetricsFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.MetricsFilter where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.MetricsAndOperator
import Network.AWS.S3.Types.Tag

-- | Specifies a metrics configuration filter. The metrics configuration only includes objects that meet the filter's criteria. A filter must be a prefix, a tag, or a conjunction (MetricsAndOperator).
--
--
--
-- /See:/ 'metricsFilter' smart constructor.
data MetricsFilter = MetricsFilter'
  { _mfTag :: !(Maybe Tag),
    _mfPrefix :: !(Maybe Text),
    _mfAnd :: !(Maybe MetricsAndOperator)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'MetricsFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mfTag' - The tag used when evaluating a metrics filter.
--
-- * 'mfPrefix' - The prefix used when evaluating a metrics filter.
--
-- * 'mfAnd' - A conjunction (logical AND) of predicates, which is used in evaluating a metrics filter. The operator must have at least two predicates, and an object must match all of the predicates in order for the filter to apply.
metricsFilter ::
  MetricsFilter
metricsFilter =
  MetricsFilter'
    { _mfTag = Nothing,
      _mfPrefix = Nothing,
      _mfAnd = Nothing
    }

-- | The tag used when evaluating a metrics filter.
mfTag :: Lens' MetricsFilter (Maybe Tag)
mfTag = lens _mfTag (\s a -> s {_mfTag = a})

-- | The prefix used when evaluating a metrics filter.
mfPrefix :: Lens' MetricsFilter (Maybe Text)
mfPrefix = lens _mfPrefix (\s a -> s {_mfPrefix = a})

-- | A conjunction (logical AND) of predicates, which is used in evaluating a metrics filter. The operator must have at least two predicates, and an object must match all of the predicates in order for the filter to apply.
mfAnd :: Lens' MetricsFilter (Maybe MetricsAndOperator)
mfAnd = lens _mfAnd (\s a -> s {_mfAnd = a})

instance FromXML MetricsFilter where
  parseXML x =
    MetricsFilter'
      <$> (x .@? "Tag") <*> (x .@? "Prefix") <*> (x .@? "And")

instance Hashable MetricsFilter

instance NFData MetricsFilter

instance ToXML MetricsFilter where
  toXML MetricsFilter' {..} =
    mconcat ["Tag" @= _mfTag, "Prefix" @= _mfPrefix, "And" @= _mfAnd]
