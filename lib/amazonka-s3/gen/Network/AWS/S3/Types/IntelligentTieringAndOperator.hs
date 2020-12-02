{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.IntelligentTieringAndOperator
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.IntelligentTieringAndOperator where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.Tag

-- | A container for specifying S3 Intelligent-Tiering filters. The filters determine the subset of objects to which the rule applies.
--
--
--
-- /See:/ 'intelligentTieringAndOperator' smart constructor.
data IntelligentTieringAndOperator = IntelligentTieringAndOperator'
  { _itaoPrefix ::
      !(Maybe Text),
    _itaoTags :: !(Maybe [Tag])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'IntelligentTieringAndOperator' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'itaoPrefix' - An object key name prefix that identifies the subset of objects to which the configuration applies.
--
-- * 'itaoTags' - All of these tags must exist in the object's tag set in order for the configuration to apply.
intelligentTieringAndOperator ::
  IntelligentTieringAndOperator
intelligentTieringAndOperator =
  IntelligentTieringAndOperator'
    { _itaoPrefix = Nothing,
      _itaoTags = Nothing
    }

-- | An object key name prefix that identifies the subset of objects to which the configuration applies.
itaoPrefix :: Lens' IntelligentTieringAndOperator (Maybe Text)
itaoPrefix = lens _itaoPrefix (\s a -> s {_itaoPrefix = a})

-- | All of these tags must exist in the object's tag set in order for the configuration to apply.
itaoTags :: Lens' IntelligentTieringAndOperator [Tag]
itaoTags = lens _itaoTags (\s a -> s {_itaoTags = a}) . _Default . _Coerce

instance FromXML IntelligentTieringAndOperator where
  parseXML x =
    IntelligentTieringAndOperator'
      <$> (x .@? "Prefix")
      <*> (x .@? "Tag" .!@ mempty >>= may (parseXMLList "Tag"))

instance Hashable IntelligentTieringAndOperator

instance NFData IntelligentTieringAndOperator

instance ToXML IntelligentTieringAndOperator where
  toXML IntelligentTieringAndOperator' {..} =
    mconcat
      [ "Prefix" @= _itaoPrefix,
        "Tag" @= toXML (toXMLList "Tag" <$> _itaoTags)
      ]
