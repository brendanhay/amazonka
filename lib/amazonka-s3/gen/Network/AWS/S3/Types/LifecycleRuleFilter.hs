{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.LifecycleRuleFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.LifecycleRuleFilter where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.LifecycleRuleAndOperator
import Network.AWS.S3.Types.Tag

-- | The @Filter@ is used to identify objects that a Lifecycle Rule applies to. A @Filter@ must have exactly one of @Prefix@ , @Tag@ , or @And@ specified.
--
--
--
-- /See:/ 'lifecycleRuleFilter' smart constructor.
data LifecycleRuleFilter = LifecycleRuleFilter'
  { _lrfTag ::
      !(Maybe Tag),
    _lrfPrefix :: !(Maybe Text),
    _lrfAnd :: !(Maybe LifecycleRuleAndOperator)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'LifecycleRuleFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lrfTag' - This tag must exist in the object's tag set in order for the rule to apply.
--
-- * 'lrfPrefix' - Prefix identifying one or more objects to which the rule applies.
--
-- * 'lrfAnd' - Undocumented member.
lifecycleRuleFilter ::
  LifecycleRuleFilter
lifecycleRuleFilter =
  LifecycleRuleFilter'
    { _lrfTag = Nothing,
      _lrfPrefix = Nothing,
      _lrfAnd = Nothing
    }

-- | This tag must exist in the object's tag set in order for the rule to apply.
lrfTag :: Lens' LifecycleRuleFilter (Maybe Tag)
lrfTag = lens _lrfTag (\s a -> s {_lrfTag = a})

-- | Prefix identifying one or more objects to which the rule applies.
lrfPrefix :: Lens' LifecycleRuleFilter (Maybe Text)
lrfPrefix = lens _lrfPrefix (\s a -> s {_lrfPrefix = a})

-- | Undocumented member.
lrfAnd :: Lens' LifecycleRuleFilter (Maybe LifecycleRuleAndOperator)
lrfAnd = lens _lrfAnd (\s a -> s {_lrfAnd = a})

instance FromXML LifecycleRuleFilter where
  parseXML x =
    LifecycleRuleFilter'
      <$> (x .@? "Tag") <*> (x .@? "Prefix") <*> (x .@? "And")

instance Hashable LifecycleRuleFilter

instance NFData LifecycleRuleFilter

instance ToXML LifecycleRuleFilter where
  toXML LifecycleRuleFilter' {..} =
    mconcat
      ["Tag" @= _lrfTag, "Prefix" @= _lrfPrefix, "And" @= _lrfAnd]
