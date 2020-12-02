{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.IntelligentTieringFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.IntelligentTieringFilter where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.IntelligentTieringAndOperator
import Network.AWS.S3.Types.Tag

-- | The @Filter@ is used to identify objects that the S3 Intelligent-Tiering configuration applies to.
--
--
--
-- /See:/ 'intelligentTieringFilter' smart constructor.
data IntelligentTieringFilter = IntelligentTieringFilter'
  { _itfTag ::
      !(Maybe Tag),
    _itfPrefix :: !(Maybe Text),
    _itfAnd ::
      !(Maybe IntelligentTieringAndOperator)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'IntelligentTieringFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'itfTag' - Undocumented member.
--
-- * 'itfPrefix' - An object key name prefix that identifies the subset of objects to which the rule applies.
--
-- * 'itfAnd' - A conjunction (logical AND) of predicates, which is used in evaluating a metrics filter. The operator must have at least two predicates, and an object must match all of the predicates in order for the filter to apply.
intelligentTieringFilter ::
  IntelligentTieringFilter
intelligentTieringFilter =
  IntelligentTieringFilter'
    { _itfTag = Nothing,
      _itfPrefix = Nothing,
      _itfAnd = Nothing
    }

-- | Undocumented member.
itfTag :: Lens' IntelligentTieringFilter (Maybe Tag)
itfTag = lens _itfTag (\s a -> s {_itfTag = a})

-- | An object key name prefix that identifies the subset of objects to which the rule applies.
itfPrefix :: Lens' IntelligentTieringFilter (Maybe Text)
itfPrefix = lens _itfPrefix (\s a -> s {_itfPrefix = a})

-- | A conjunction (logical AND) of predicates, which is used in evaluating a metrics filter. The operator must have at least two predicates, and an object must match all of the predicates in order for the filter to apply.
itfAnd :: Lens' IntelligentTieringFilter (Maybe IntelligentTieringAndOperator)
itfAnd = lens _itfAnd (\s a -> s {_itfAnd = a})

instance FromXML IntelligentTieringFilter where
  parseXML x =
    IntelligentTieringFilter'
      <$> (x .@? "Tag") <*> (x .@? "Prefix") <*> (x .@? "And")

instance Hashable IntelligentTieringFilter

instance NFData IntelligentTieringFilter

instance ToXML IntelligentTieringFilter where
  toXML IntelligentTieringFilter' {..} =
    mconcat
      ["Tag" @= _itfTag, "Prefix" @= _itfPrefix, "And" @= _itfAnd]
