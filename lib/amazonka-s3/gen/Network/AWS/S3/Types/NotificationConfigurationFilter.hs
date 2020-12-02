{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.NotificationConfigurationFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.NotificationConfigurationFilter where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.S3KeyFilter

-- | Specifies object key name filtering rules. For information about key name filtering, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/NotificationHowTo.html Configuring Event Notifications> in the /Amazon Simple Storage Service Developer Guide/ .
--
--
--
-- /See:/ 'notificationConfigurationFilter' smart constructor.
newtype NotificationConfigurationFilter = NotificationConfigurationFilter'
  { _ncfKey ::
      Maybe S3KeyFilter
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'NotificationConfigurationFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ncfKey' - Undocumented member.
notificationConfigurationFilter ::
  NotificationConfigurationFilter
notificationConfigurationFilter =
  NotificationConfigurationFilter' {_ncfKey = Nothing}

-- | Undocumented member.
ncfKey :: Lens' NotificationConfigurationFilter (Maybe S3KeyFilter)
ncfKey = lens _ncfKey (\s a -> s {_ncfKey = a})

instance FromXML NotificationConfigurationFilter where
  parseXML x = NotificationConfigurationFilter' <$> (x .@? "S3Key")

instance Hashable NotificationConfigurationFilter

instance NFData NotificationConfigurationFilter

instance ToXML NotificationConfigurationFilter where
  toXML NotificationConfigurationFilter' {..} =
    mconcat ["S3Key" @= _ncfKey]
