{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.ObjectLockRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.ObjectLockRule where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.DefaultRetention

-- | The container element for an Object Lock rule.
--
--
--
-- /See:/ 'objectLockRule' smart constructor.
newtype ObjectLockRule = ObjectLockRule'
  { _olrDefaultRetention ::
      Maybe DefaultRetention
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ObjectLockRule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'olrDefaultRetention' - The default retention period that you want to apply to new objects placed in the specified bucket.
objectLockRule ::
  ObjectLockRule
objectLockRule = ObjectLockRule' {_olrDefaultRetention = Nothing}

-- | The default retention period that you want to apply to new objects placed in the specified bucket.
olrDefaultRetention :: Lens' ObjectLockRule (Maybe DefaultRetention)
olrDefaultRetention = lens _olrDefaultRetention (\s a -> s {_olrDefaultRetention = a})

instance FromXML ObjectLockRule where
  parseXML x = ObjectLockRule' <$> (x .@? "DefaultRetention")

instance Hashable ObjectLockRule

instance NFData ObjectLockRule

instance ToXML ObjectLockRule where
  toXML ObjectLockRule' {..} =
    mconcat ["DefaultRetention" @= _olrDefaultRetention]
