{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.MetricsAndOperator
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.MetricsAndOperator where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.Tag

-- | A conjunction (logical AND) of predicates, which is used in evaluating a metrics filter. The operator must have at least two predicates, and an object must match all of the predicates in order for the filter to apply.
--
--
--
-- /See:/ 'metricsAndOperator' smart constructor.
data MetricsAndOperator = MetricsAndOperator'
  { _maoPrefix ::
      !(Maybe Text),
    _maoTags :: !(Maybe [Tag])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'MetricsAndOperator' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'maoPrefix' - The prefix used when evaluating an AND predicate.
--
-- * 'maoTags' - The list of tags used when evaluating an AND predicate.
metricsAndOperator ::
  MetricsAndOperator
metricsAndOperator =
  MetricsAndOperator' {_maoPrefix = Nothing, _maoTags = Nothing}

-- | The prefix used when evaluating an AND predicate.
maoPrefix :: Lens' MetricsAndOperator (Maybe Text)
maoPrefix = lens _maoPrefix (\s a -> s {_maoPrefix = a})

-- | The list of tags used when evaluating an AND predicate.
maoTags :: Lens' MetricsAndOperator [Tag]
maoTags = lens _maoTags (\s a -> s {_maoTags = a}) . _Default . _Coerce

instance FromXML MetricsAndOperator where
  parseXML x =
    MetricsAndOperator'
      <$> (x .@? "Prefix")
      <*> (x .@? "Tag" .!@ mempty >>= may (parseXMLList "Tag"))

instance Hashable MetricsAndOperator

instance NFData MetricsAndOperator

instance ToXML MetricsAndOperator where
  toXML MetricsAndOperator' {..} =
    mconcat
      [ "Prefix" @= _maoPrefix,
        "Tag" @= toXML (toXMLList "Tag" <$> _maoTags)
      ]
