{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.Owner
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.Owner where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.S3.Internal

-- | Container for the owner's display name and ID.
--
--
--
-- /See:/ 'owner' smart constructor.
data Owner = Owner'
  { _oDisplayName :: !(Maybe Text),
    _oId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Owner' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'oDisplayName' - Container for the display name of the owner.
--
-- * 'oId' - Container for the ID of the owner.
owner ::
  Owner
owner = Owner' {_oDisplayName = Nothing, _oId = Nothing}

-- | Container for the display name of the owner.
oDisplayName :: Lens' Owner (Maybe Text)
oDisplayName = lens _oDisplayName (\s a -> s {_oDisplayName = a})

-- | Container for the ID of the owner.
oId :: Lens' Owner (Maybe Text)
oId = lens _oId (\s a -> s {_oId = a})

instance FromXML Owner where
  parseXML x = Owner' <$> (x .@? "DisplayName") <*> (x .@? "ID")

instance Hashable Owner

instance NFData Owner

instance ToXML Owner where
  toXML Owner' {..} =
    mconcat ["DisplayName" @= _oDisplayName, "ID" @= _oId]
