{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.OidcMemberDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.OidcMemberDefinition where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A list of user groups that exist in your OIDC Identity Provider (IdP). One to ten groups can be used to create a single private work team. When you add a user group to the list of @Groups@ , you can add that user group to one or more private work teams. If you add a user group to a private work team, all workers in that user group are added to the work team.
--
--
--
-- /See:/ 'oidcMemberDefinition' smart constructor.
newtype OidcMemberDefinition = OidcMemberDefinition'
  { _omdGroups ::
      List1 Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'OidcMemberDefinition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'omdGroups' - A list of comma seperated strings that identifies user groups in your OIDC IdP. Each user group is made up of a group of private workers.
oidcMemberDefinition ::
  -- | 'omdGroups'
  NonEmpty Text ->
  OidcMemberDefinition
oidcMemberDefinition pGroups_ =
  OidcMemberDefinition' {_omdGroups = _List1 # pGroups_}

-- | A list of comma seperated strings that identifies user groups in your OIDC IdP. Each user group is made up of a group of private workers.
omdGroups :: Lens' OidcMemberDefinition (NonEmpty Text)
omdGroups = lens _omdGroups (\s a -> s {_omdGroups = a}) . _List1

instance FromJSON OidcMemberDefinition where
  parseJSON =
    withObject
      "OidcMemberDefinition"
      (\x -> OidcMemberDefinition' <$> (x .: "Groups"))

instance Hashable OidcMemberDefinition

instance NFData OidcMemberDefinition

instance ToJSON OidcMemberDefinition where
  toJSON OidcMemberDefinition' {..} =
    object (catMaybes [Just ("Groups" .= _omdGroups)])
