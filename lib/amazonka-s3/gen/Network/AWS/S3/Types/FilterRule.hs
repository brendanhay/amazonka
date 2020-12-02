{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.FilterRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.FilterRule where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.FilterRuleName

-- | Specifies the Amazon S3 object key name to filter on and whether to filter on the suffix or prefix of the key name.
--
--
--
-- /See:/ 'filterRule' smart constructor.
data FilterRule = FilterRule'
  { _frValue :: !(Maybe Text),
    _frName :: !(Maybe FilterRuleName)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'FilterRule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'frValue' - The value that the filter searches for in object key names.
--
-- * 'frName' - The object key name prefix or suffix identifying one or more objects to which the filtering rule applies. The maximum length is 1,024 characters. Overlapping prefixes and suffixes are not supported. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/NotificationHowTo.html Configuring Event Notifications> in the /Amazon Simple Storage Service Developer Guide/ .
filterRule ::
  FilterRule
filterRule = FilterRule' {_frValue = Nothing, _frName = Nothing}

-- | The value that the filter searches for in object key names.
frValue :: Lens' FilterRule (Maybe Text)
frValue = lens _frValue (\s a -> s {_frValue = a})

-- | The object key name prefix or suffix identifying one or more objects to which the filtering rule applies. The maximum length is 1,024 characters. Overlapping prefixes and suffixes are not supported. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/NotificationHowTo.html Configuring Event Notifications> in the /Amazon Simple Storage Service Developer Guide/ .
frName :: Lens' FilterRule (Maybe FilterRuleName)
frName = lens _frName (\s a -> s {_frName = a})

instance FromXML FilterRule where
  parseXML x = FilterRule' <$> (x .@? "Value") <*> (x .@? "Name")

instance Hashable FilterRule

instance NFData FilterRule

instance ToXML FilterRule where
  toXML FilterRule' {..} =
    mconcat ["Value" @= _frValue, "Name" @= _frName]
