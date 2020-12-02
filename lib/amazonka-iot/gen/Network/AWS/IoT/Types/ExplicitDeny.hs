{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.ExplicitDeny
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.ExplicitDeny where

import Network.AWS.IoT.Types.Policy
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information that explicitly denies authorization.
--
--
--
-- /See:/ 'explicitDeny' smart constructor.
newtype ExplicitDeny = ExplicitDeny' {_edPolicies :: Maybe [Policy]}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ExplicitDeny' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'edPolicies' - The policies that denied the authorization.
explicitDeny ::
  ExplicitDeny
explicitDeny = ExplicitDeny' {_edPolicies = Nothing}

-- | The policies that denied the authorization.
edPolicies :: Lens' ExplicitDeny [Policy]
edPolicies = lens _edPolicies (\s a -> s {_edPolicies = a}) . _Default . _Coerce

instance FromJSON ExplicitDeny where
  parseJSON =
    withObject
      "ExplicitDeny"
      (\x -> ExplicitDeny' <$> (x .:? "policies" .!= mempty))

instance Hashable ExplicitDeny

instance NFData ExplicitDeny
