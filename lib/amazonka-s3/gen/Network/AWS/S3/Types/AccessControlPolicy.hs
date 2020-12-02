{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.AccessControlPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.AccessControlPolicy where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.Grant
import Network.AWS.S3.Types.Owner

-- | Contains the elements that set the ACL permissions for an object per grantee.
--
--
--
-- /See:/ 'accessControlPolicy' smart constructor.
data AccessControlPolicy = AccessControlPolicy'
  { _acpGrants ::
      !(Maybe [Grant]),
    _acpOwner :: !(Maybe Owner)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AccessControlPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'acpGrants' - A list of grants.
--
-- * 'acpOwner' - Container for the bucket owner's display name and ID.
accessControlPolicy ::
  AccessControlPolicy
accessControlPolicy =
  AccessControlPolicy' {_acpGrants = Nothing, _acpOwner = Nothing}

-- | A list of grants.
acpGrants :: Lens' AccessControlPolicy [Grant]
acpGrants = lens _acpGrants (\s a -> s {_acpGrants = a}) . _Default . _Coerce

-- | Container for the bucket owner's display name and ID.
acpOwner :: Lens' AccessControlPolicy (Maybe Owner)
acpOwner = lens _acpOwner (\s a -> s {_acpOwner = a})

instance Hashable AccessControlPolicy

instance NFData AccessControlPolicy

instance ToXML AccessControlPolicy where
  toXML AccessControlPolicy' {..} =
    mconcat
      [ "AccessControlList" @= toXML (toXMLList "Grant" <$> _acpGrants),
        "Owner" @= _acpOwner
      ]
