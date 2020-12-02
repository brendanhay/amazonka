{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.ObjectIdentifier
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.ObjectIdentifier where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.S3.Internal

-- | Object Identifier is unique value to identify objects.
--
--
--
-- /See:/ 'objectIdentifier' smart constructor.
data ObjectIdentifier = ObjectIdentifier'
  { _oiVersionId ::
      !(Maybe ObjectVersionId),
    _oiKey :: !ObjectKey
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ObjectIdentifier' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'oiVersionId' - VersionId for the specific version of the object to delete.
--
-- * 'oiKey' - Key name of the object to delete.
objectIdentifier ::
  -- | 'oiKey'
  ObjectKey ->
  ObjectIdentifier
objectIdentifier pKey_ =
  ObjectIdentifier' {_oiVersionId = Nothing, _oiKey = pKey_}

-- | VersionId for the specific version of the object to delete.
oiVersionId :: Lens' ObjectIdentifier (Maybe ObjectVersionId)
oiVersionId = lens _oiVersionId (\s a -> s {_oiVersionId = a})

-- | Key name of the object to delete.
oiKey :: Lens' ObjectIdentifier ObjectKey
oiKey = lens _oiKey (\s a -> s {_oiKey = a})

instance Hashable ObjectIdentifier

instance NFData ObjectIdentifier

instance ToXML ObjectIdentifier where
  toXML ObjectIdentifier' {..} =
    mconcat ["VersionId" @= _oiVersionId, "Key" @= _oiKey]
