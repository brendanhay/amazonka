{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.LifecycleRuleAndOperator
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.LifecycleRuleAndOperator where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.Tag

-- | This is used in a Lifecycle Rule Filter to apply a logical AND to two or more predicates. The Lifecycle Rule will apply to any object matching all of the predicates configured inside the And operator.
--
--
--
-- /See:/ 'lifecycleRuleAndOperator' smart constructor.
data LifecycleRuleAndOperator = LifecycleRuleAndOperator'
  { _lraoPrefix ::
      !(Maybe Text),
    _lraoTags :: !(Maybe [Tag])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'LifecycleRuleAndOperator' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lraoPrefix' - Prefix identifying one or more objects to which the rule applies.
--
-- * 'lraoTags' - All of these tags must exist in the object's tag set in order for the rule to apply.
lifecycleRuleAndOperator ::
  LifecycleRuleAndOperator
lifecycleRuleAndOperator =
  LifecycleRuleAndOperator'
    { _lraoPrefix = Nothing,
      _lraoTags = Nothing
    }

-- | Prefix identifying one or more objects to which the rule applies.
lraoPrefix :: Lens' LifecycleRuleAndOperator (Maybe Text)
lraoPrefix = lens _lraoPrefix (\s a -> s {_lraoPrefix = a})

-- | All of these tags must exist in the object's tag set in order for the rule to apply.
lraoTags :: Lens' LifecycleRuleAndOperator [Tag]
lraoTags = lens _lraoTags (\s a -> s {_lraoTags = a}) . _Default . _Coerce

instance FromXML LifecycleRuleAndOperator where
  parseXML x =
    LifecycleRuleAndOperator'
      <$> (x .@? "Prefix")
      <*> (x .@? "Tag" .!@ mempty >>= may (parseXMLList "Tag"))

instance Hashable LifecycleRuleAndOperator

instance NFData LifecycleRuleAndOperator

instance ToXML LifecycleRuleAndOperator where
  toXML LifecycleRuleAndOperator' {..} =
    mconcat
      [ "Prefix" @= _lraoPrefix,
        "Tag" @= toXML (toXMLList "Tag" <$> _lraoTags)
      ]
