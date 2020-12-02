{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.Allowed
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.Allowed where

import Network.AWS.IoT.Types.Policy
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information that allowed the authorization.
--
--
--
-- /See:/ 'allowed' smart constructor.
newtype Allowed = Allowed' {_aPolicies :: Maybe [Policy]}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Allowed' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aPolicies' - A list of policies that allowed the authentication.
allowed ::
  Allowed
allowed = Allowed' {_aPolicies = Nothing}

-- | A list of policies that allowed the authentication.
aPolicies :: Lens' Allowed [Policy]
aPolicies = lens _aPolicies (\s a -> s {_aPolicies = a}) . _Default . _Coerce

instance FromJSON Allowed where
  parseJSON =
    withObject
      "Allowed"
      (\x -> Allowed' <$> (x .:? "policies" .!= mempty))

instance Hashable Allowed

instance NFData Allowed
