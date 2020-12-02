{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.Delete
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.Delete where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.ObjectIdentifier

-- | Container for the objects to delete.
--
--
--
-- /See:/ 'delete'' smart constructor.
data Delete = Delete'
  { _dQuiet :: !(Maybe Bool),
    _dObjects :: ![ObjectIdentifier]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Delete' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dQuiet' - Element to enable quiet mode for the request. When you add this element, you must set its value to true.
--
-- * 'dObjects' - The objects to delete.
delete' ::
  Delete
delete' = Delete' {_dQuiet = Nothing, _dObjects = mempty}

-- | Element to enable quiet mode for the request. When you add this element, you must set its value to true.
dQuiet :: Lens' Delete (Maybe Bool)
dQuiet = lens _dQuiet (\s a -> s {_dQuiet = a})

-- | The objects to delete.
dObjects :: Lens' Delete [ObjectIdentifier]
dObjects = lens _dObjects (\s a -> s {_dObjects = a}) . _Coerce

instance Hashable Delete

instance NFData Delete

instance ToXML Delete where
  toXML Delete' {..} =
    mconcat ["Quiet" @= _dQuiet, toXMLList "Object" _dObjects]
