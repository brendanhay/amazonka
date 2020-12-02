{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.DefaultRetention
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.DefaultRetention where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.ObjectLockRetentionMode

-- | The container element for specifying the default Object Lock retention settings for new objects placed in the specified bucket.
--
--
--
-- /See:/ 'defaultRetention' smart constructor.
data DefaultRetention = DefaultRetention'
  { _drDays :: !(Maybe Int),
    _drMode :: !(Maybe ObjectLockRetentionMode),
    _drYears :: !(Maybe Int)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DefaultRetention' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drDays' - The number of days that you want to specify for the default retention period.
--
-- * 'drMode' - The default Object Lock retention mode you want to apply to new objects placed in the specified bucket.
--
-- * 'drYears' - The number of years that you want to specify for the default retention period.
defaultRetention ::
  DefaultRetention
defaultRetention =
  DefaultRetention'
    { _drDays = Nothing,
      _drMode = Nothing,
      _drYears = Nothing
    }

-- | The number of days that you want to specify for the default retention period.
drDays :: Lens' DefaultRetention (Maybe Int)
drDays = lens _drDays (\s a -> s {_drDays = a})

-- | The default Object Lock retention mode you want to apply to new objects placed in the specified bucket.
drMode :: Lens' DefaultRetention (Maybe ObjectLockRetentionMode)
drMode = lens _drMode (\s a -> s {_drMode = a})

-- | The number of years that you want to specify for the default retention period.
drYears :: Lens' DefaultRetention (Maybe Int)
drYears = lens _drYears (\s a -> s {_drYears = a})

instance FromXML DefaultRetention where
  parseXML x =
    DefaultRetention'
      <$> (x .@? "Days") <*> (x .@? "Mode") <*> (x .@? "Years")

instance Hashable DefaultRetention

instance NFData DefaultRetention

instance ToXML DefaultRetention where
  toXML DefaultRetention' {..} =
    mconcat
      ["Days" @= _drDays, "Mode" @= _drMode, "Years" @= _drYears]
