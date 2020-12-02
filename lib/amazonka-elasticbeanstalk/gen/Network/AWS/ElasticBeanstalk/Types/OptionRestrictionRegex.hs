{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.OptionRestrictionRegex
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.OptionRestrictionRegex where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A regular expression representing a restriction on a string configuration option value.
--
--
--
-- /See:/ 'optionRestrictionRegex' smart constructor.
data OptionRestrictionRegex = OptionRestrictionRegex'
  { _orrPattern ::
      !(Maybe Text),
    _orrLabel :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'OptionRestrictionRegex' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'orrPattern' - The regular expression pattern that a string configuration option value with this restriction must match.
--
-- * 'orrLabel' - A unique name representing this regular expression.
optionRestrictionRegex ::
  OptionRestrictionRegex
optionRestrictionRegex =
  OptionRestrictionRegex'
    { _orrPattern = Nothing,
      _orrLabel = Nothing
    }

-- | The regular expression pattern that a string configuration option value with this restriction must match.
orrPattern :: Lens' OptionRestrictionRegex (Maybe Text)
orrPattern = lens _orrPattern (\s a -> s {_orrPattern = a})

-- | A unique name representing this regular expression.
orrLabel :: Lens' OptionRestrictionRegex (Maybe Text)
orrLabel = lens _orrLabel (\s a -> s {_orrLabel = a})

instance FromXML OptionRestrictionRegex where
  parseXML x =
    OptionRestrictionRegex' <$> (x .@? "Pattern") <*> (x .@? "Label")

instance Hashable OptionRestrictionRegex

instance NFData OptionRestrictionRegex
