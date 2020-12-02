{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.Denied
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.Denied where

import Network.AWS.IoT.Types.ExplicitDeny
import Network.AWS.IoT.Types.ImplicitDeny
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information that denied the authorization.
--
--
--
-- /See:/ 'denied' smart constructor.
data Denied = Denied'
  { _dImplicitDeny :: !(Maybe ImplicitDeny),
    _dExplicitDeny :: !(Maybe ExplicitDeny)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Denied' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dImplicitDeny' - Information that implicitly denies the authorization. When a policy doesn't explicitly deny or allow an action on a resource it is considered an implicit deny.
--
-- * 'dExplicitDeny' - Information that explicitly denies the authorization.
denied ::
  Denied
denied =
  Denied' {_dImplicitDeny = Nothing, _dExplicitDeny = Nothing}

-- | Information that implicitly denies the authorization. When a policy doesn't explicitly deny or allow an action on a resource it is considered an implicit deny.
dImplicitDeny :: Lens' Denied (Maybe ImplicitDeny)
dImplicitDeny = lens _dImplicitDeny (\s a -> s {_dImplicitDeny = a})

-- | Information that explicitly denies the authorization.
dExplicitDeny :: Lens' Denied (Maybe ExplicitDeny)
dExplicitDeny = lens _dExplicitDeny (\s a -> s {_dExplicitDeny = a})

instance FromJSON Denied where
  parseJSON =
    withObject
      "Denied"
      ( \x ->
          Denied' <$> (x .:? "implicitDeny") <*> (x .:? "explicitDeny")
      )

instance Hashable Denied

instance NFData Denied
