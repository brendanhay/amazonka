{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkMail.Types.Permission
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkMail.Types.Permission where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.WorkMail.Types.MemberType
import Network.AWS.WorkMail.Types.PermissionType

-- | Permission granted to a user, group, or resource to access a certain aspect of another user, group, or resource mailbox.
--
--
--
-- /See:/ 'permission' smart constructor.
data Permission = Permission'
  { _pGranteeId :: !Text,
    _pGranteeType :: !MemberType,
    _pPermissionValues :: ![PermissionType]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Permission' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pGranteeId' - The identifier of the user, group, or resource to which the permissions are granted.
--
-- * 'pGranteeType' - The type of user, group, or resource referred to in GranteeId.
--
-- * 'pPermissionValues' - The permissions granted to the grantee. SEND_AS allows the grantee to send email as the owner of the mailbox (the grantee is not mentioned on these emails). SEND_ON_BEHALF allows the grantee to send email on behalf of the owner of the mailbox (the grantee is not mentioned as the physical sender of these emails). FULL_ACCESS allows the grantee full access to the mailbox, irrespective of other folder-level permissions set on the mailbox.
permission ::
  -- | 'pGranteeId'
  Text ->
  -- | 'pGranteeType'
  MemberType ->
  Permission
permission pGranteeId_ pGranteeType_ =
  Permission'
    { _pGranteeId = pGranteeId_,
      _pGranteeType = pGranteeType_,
      _pPermissionValues = mempty
    }

-- | The identifier of the user, group, or resource to which the permissions are granted.
pGranteeId :: Lens' Permission Text
pGranteeId = lens _pGranteeId (\s a -> s {_pGranteeId = a})

-- | The type of user, group, or resource referred to in GranteeId.
pGranteeType :: Lens' Permission MemberType
pGranteeType = lens _pGranteeType (\s a -> s {_pGranteeType = a})

-- | The permissions granted to the grantee. SEND_AS allows the grantee to send email as the owner of the mailbox (the grantee is not mentioned on these emails). SEND_ON_BEHALF allows the grantee to send email on behalf of the owner of the mailbox (the grantee is not mentioned as the physical sender of these emails). FULL_ACCESS allows the grantee full access to the mailbox, irrespective of other folder-level permissions set on the mailbox.
pPermissionValues :: Lens' Permission [PermissionType]
pPermissionValues = lens _pPermissionValues (\s a -> s {_pPermissionValues = a}) . _Coerce

instance FromJSON Permission where
  parseJSON =
    withObject
      "Permission"
      ( \x ->
          Permission'
            <$> (x .: "GranteeId")
            <*> (x .: "GranteeType")
            <*> (x .:? "PermissionValues" .!= mempty)
      )

instance Hashable Permission

instance NFData Permission
