{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.Types.Grant
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glacier.Types.Grant where

import Network.AWS.Glacier.Types.Grantee
import Network.AWS.Glacier.Types.Permission
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information about a grant.
--
--
--
-- /See:/ 'grant' smart constructor.
data Grant = Grant'
  { _gPermission :: !(Maybe Permission),
    _gGrantee :: !(Maybe Grantee)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Grant' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gPermission' - Specifies the permission given to the grantee.
--
-- * 'gGrantee' - The grantee.
grant ::
  Grant
grant = Grant' {_gPermission = Nothing, _gGrantee = Nothing}

-- | Specifies the permission given to the grantee.
gPermission :: Lens' Grant (Maybe Permission)
gPermission = lens _gPermission (\s a -> s {_gPermission = a})

-- | The grantee.
gGrantee :: Lens' Grant (Maybe Grantee)
gGrantee = lens _gGrantee (\s a -> s {_gGrantee = a})

instance FromJSON Grant where
  parseJSON =
    withObject
      "Grant"
      (\x -> Grant' <$> (x .:? "Permission") <*> (x .:? "Grantee"))

instance Hashable Grant

instance NFData Grant

instance ToJSON Grant where
  toJSON Grant' {..} =
    object
      ( catMaybes
          [("Permission" .=) <$> _gPermission, ("Grantee" .=) <$> _gGrantee]
      )
