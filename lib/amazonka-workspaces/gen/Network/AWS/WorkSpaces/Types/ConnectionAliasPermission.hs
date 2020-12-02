{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.Types.ConnectionAliasPermission
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkSpaces.Types.ConnectionAliasPermission where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the permissions for a connection alias. Connection aliases are used for cross-Region redirection. For more information, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/cross-region-redirection.html Cross-Region Redirection for Amazon WorkSpaces> .
--
--
--
-- /See:/ 'connectionAliasPermission' smart constructor.
data ConnectionAliasPermission = ConnectionAliasPermission'
  { _capSharedAccountId ::
      !Text,
    _capAllowAssociation :: !Bool
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ConnectionAliasPermission' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'capSharedAccountId' - The identifier of the AWS account that the connection alias is shared with.
--
-- * 'capAllowAssociation' - Indicates whether the specified AWS account is allowed to associate the connection alias with a directory.
connectionAliasPermission ::
  -- | 'capSharedAccountId'
  Text ->
  -- | 'capAllowAssociation'
  Bool ->
  ConnectionAliasPermission
connectionAliasPermission pSharedAccountId_ pAllowAssociation_ =
  ConnectionAliasPermission'
    { _capSharedAccountId =
        pSharedAccountId_,
      _capAllowAssociation = pAllowAssociation_
    }

-- | The identifier of the AWS account that the connection alias is shared with.
capSharedAccountId :: Lens' ConnectionAliasPermission Text
capSharedAccountId = lens _capSharedAccountId (\s a -> s {_capSharedAccountId = a})

-- | Indicates whether the specified AWS account is allowed to associate the connection alias with a directory.
capAllowAssociation :: Lens' ConnectionAliasPermission Bool
capAllowAssociation = lens _capAllowAssociation (\s a -> s {_capAllowAssociation = a})

instance FromJSON ConnectionAliasPermission where
  parseJSON =
    withObject
      "ConnectionAliasPermission"
      ( \x ->
          ConnectionAliasPermission'
            <$> (x .: "SharedAccountId") <*> (x .: "AllowAssociation")
      )

instance Hashable ConnectionAliasPermission

instance NFData ConnectionAliasPermission

instance ToJSON ConnectionAliasPermission where
  toJSON ConnectionAliasPermission' {..} =
    object
      ( catMaybes
          [ Just ("SharedAccountId" .= _capSharedAccountId),
            Just ("AllowAssociation" .= _capAllowAssociation)
          ]
      )
