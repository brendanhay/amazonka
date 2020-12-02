{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.Types.SharePrincipal
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkDocs.Types.SharePrincipal where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.WorkDocs.Types.PrincipalType
import Network.AWS.WorkDocs.Types.RoleType

-- | Describes the recipient type and ID, if available.
--
--
--
-- /See:/ 'sharePrincipal' smart constructor.
data SharePrincipal = SharePrincipal'
  { _spId :: !Text,
    _spType :: !PrincipalType,
    _spRole :: !RoleType
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SharePrincipal' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'spId' - The ID of the recipient.
--
-- * 'spType' - The type of the recipient.
--
-- * 'spRole' - The role of the recipient.
sharePrincipal ::
  -- | 'spId'
  Text ->
  -- | 'spType'
  PrincipalType ->
  -- | 'spRole'
  RoleType ->
  SharePrincipal
sharePrincipal pId_ pType_ pRole_ =
  SharePrincipal' {_spId = pId_, _spType = pType_, _spRole = pRole_}

-- | The ID of the recipient.
spId :: Lens' SharePrincipal Text
spId = lens _spId (\s a -> s {_spId = a})

-- | The type of the recipient.
spType :: Lens' SharePrincipal PrincipalType
spType = lens _spType (\s a -> s {_spType = a})

-- | The role of the recipient.
spRole :: Lens' SharePrincipal RoleType
spRole = lens _spRole (\s a -> s {_spRole = a})

instance Hashable SharePrincipal

instance NFData SharePrincipal

instance ToJSON SharePrincipal where
  toJSON SharePrincipal' {..} =
    object
      ( catMaybes
          [ Just ("Id" .= _spId),
            Just ("Type" .= _spType),
            Just ("Role" .= _spRole)
          ]
      )
