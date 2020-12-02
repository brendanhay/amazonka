{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.Types.Principal
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkDocs.Types.Principal where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.WorkDocs.Types.PermissionInfo
import Network.AWS.WorkDocs.Types.PrincipalType

-- | Describes a resource.
--
--
--
-- /See:/ 'principal' smart constructor.
data Principal = Principal'
  { _pRoles :: !(Maybe [PermissionInfo]),
    _pId :: !(Maybe Text),
    _pType :: !(Maybe PrincipalType)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Principal' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pRoles' - The permission information for the resource.
--
-- * 'pId' - The ID of the resource.
--
-- * 'pType' - The type of resource.
principal ::
  Principal
principal =
  Principal' {_pRoles = Nothing, _pId = Nothing, _pType = Nothing}

-- | The permission information for the resource.
pRoles :: Lens' Principal [PermissionInfo]
pRoles = lens _pRoles (\s a -> s {_pRoles = a}) . _Default . _Coerce

-- | The ID of the resource.
pId :: Lens' Principal (Maybe Text)
pId = lens _pId (\s a -> s {_pId = a})

-- | The type of resource.
pType :: Lens' Principal (Maybe PrincipalType)
pType = lens _pType (\s a -> s {_pType = a})

instance FromJSON Principal where
  parseJSON =
    withObject
      "Principal"
      ( \x ->
          Principal'
            <$> (x .:? "Roles" .!= mempty) <*> (x .:? "Id") <*> (x .:? "Type")
      )

instance Hashable Principal

instance NFData Principal
