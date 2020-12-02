{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.ObjectLockConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.ObjectLockConfiguration where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.ObjectLockEnabled
import Network.AWS.S3.Types.ObjectLockRule

-- | The container element for Object Lock configuration parameters.
--
--
--
-- /See:/ 'objectLockConfiguration' smart constructor.
data ObjectLockConfiguration = ObjectLockConfiguration'
  { _olcObjectLockEnabled ::
      !(Maybe ObjectLockEnabled),
    _olcRule :: !(Maybe ObjectLockRule)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ObjectLockConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'olcObjectLockEnabled' - Indicates whether this bucket has an Object Lock configuration enabled.
--
-- * 'olcRule' - The Object Lock rule in place for the specified object.
objectLockConfiguration ::
  ObjectLockConfiguration
objectLockConfiguration =
  ObjectLockConfiguration'
    { _olcObjectLockEnabled = Nothing,
      _olcRule = Nothing
    }

-- | Indicates whether this bucket has an Object Lock configuration enabled.
olcObjectLockEnabled :: Lens' ObjectLockConfiguration (Maybe ObjectLockEnabled)
olcObjectLockEnabled = lens _olcObjectLockEnabled (\s a -> s {_olcObjectLockEnabled = a})

-- | The Object Lock rule in place for the specified object.
olcRule :: Lens' ObjectLockConfiguration (Maybe ObjectLockRule)
olcRule = lens _olcRule (\s a -> s {_olcRule = a})

instance FromXML ObjectLockConfiguration where
  parseXML x =
    ObjectLockConfiguration'
      <$> (x .@? "ObjectLockEnabled") <*> (x .@? "Rule")

instance Hashable ObjectLockConfiguration

instance NFData ObjectLockConfiguration

instance ToXML ObjectLockConfiguration where
  toXML ObjectLockConfiguration' {..} =
    mconcat
      ["ObjectLockEnabled" @= _olcObjectLockEnabled, "Rule" @= _olcRule]
