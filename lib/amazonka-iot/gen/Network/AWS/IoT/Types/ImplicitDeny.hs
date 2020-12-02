{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.ImplicitDeny
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.ImplicitDeny where

import Network.AWS.IoT.Types.Policy
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information that implicitly denies authorization. When policy doesn't explicitly deny or allow an action on a resource it is considered an implicit deny.
--
--
--
-- /See:/ 'implicitDeny' smart constructor.
newtype ImplicitDeny = ImplicitDeny' {_idPolicies :: Maybe [Policy]}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ImplicitDeny' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'idPolicies' - Policies that don't contain a matching allow or deny statement for the specified action on the specified resource.
implicitDeny ::
  ImplicitDeny
implicitDeny = ImplicitDeny' {_idPolicies = Nothing}

-- | Policies that don't contain a matching allow or deny statement for the specified action on the specified resource.
idPolicies :: Lens' ImplicitDeny [Policy]
idPolicies = lens _idPolicies (\s a -> s {_idPolicies = a}) . _Default . _Coerce

instance FromJSON ImplicitDeny where
  parseJSON =
    withObject
      "ImplicitDeny"
      (\x -> ImplicitDeny' <$> (x .:? "policies" .!= mempty))

instance Hashable ImplicitDeny

instance NFData ImplicitDeny
