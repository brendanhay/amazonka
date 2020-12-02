{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.ReplicationRuleAndOperator
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.ReplicationRuleAndOperator where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.Tag

-- | A container for specifying rule filters. The filters determine the subset of objects to which the rule applies. This element is required only if you specify more than one filter.
--
--
-- For example:
--
--     * If you specify both a @Prefix@ and a @Tag@ filter, wrap these filters in an @And@ tag.
--
--     * If you specify a filter based on multiple tags, wrap the @Tag@ elements in an @And@ tag
--
--
--
--
-- /See:/ 'replicationRuleAndOperator' smart constructor.
data ReplicationRuleAndOperator = ReplicationRuleAndOperator'
  { _rraoPrefix ::
      !(Maybe Text),
    _rraoTags :: !(Maybe [Tag])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ReplicationRuleAndOperator' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rraoPrefix' - An object key name prefix that identifies the subset of objects to which the rule applies.
--
-- * 'rraoTags' - An array of tags containing key and value pairs.
replicationRuleAndOperator ::
  ReplicationRuleAndOperator
replicationRuleAndOperator =
  ReplicationRuleAndOperator'
    { _rraoPrefix = Nothing,
      _rraoTags = Nothing
    }

-- | An object key name prefix that identifies the subset of objects to which the rule applies.
rraoPrefix :: Lens' ReplicationRuleAndOperator (Maybe Text)
rraoPrefix = lens _rraoPrefix (\s a -> s {_rraoPrefix = a})

-- | An array of tags containing key and value pairs.
rraoTags :: Lens' ReplicationRuleAndOperator [Tag]
rraoTags = lens _rraoTags (\s a -> s {_rraoTags = a}) . _Default . _Coerce

instance FromXML ReplicationRuleAndOperator where
  parseXML x =
    ReplicationRuleAndOperator'
      <$> (x .@? "Prefix")
      <*> (x .@? "Tag" .!@ mempty >>= may (parseXMLList "Tag"))

instance Hashable ReplicationRuleAndOperator

instance NFData ReplicationRuleAndOperator

instance ToXML ReplicationRuleAndOperator where
  toXML ReplicationRuleAndOperator' {..} =
    mconcat
      [ "Prefix" @= _rraoPrefix,
        "Tag" @= toXML (toXMLList "Tag" <$> _rraoTags)
      ]
