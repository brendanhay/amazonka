{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.DeletionTaskFailureReasonType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.DeletionTaskFailureReasonType where

import Network.AWS.IAM.Types.RoleUsageType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The reason that the service-linked role deletion failed.
--
--
-- This data type is used as a response element in the 'GetServiceLinkedRoleDeletionStatus' operation.
--
--
-- /See:/ 'deletionTaskFailureReasonType' smart constructor.
data DeletionTaskFailureReasonType = DeletionTaskFailureReasonType'
  { _dtfrtRoleUsageList ::
      !(Maybe [RoleUsageType]),
    _dtfrtReason :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeletionTaskFailureReasonType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtfrtRoleUsageList' - A list of objects that contains details about the service-linked role deletion failure, if that information is returned by the service. If the service-linked role has active sessions or if any resources that were used by the role have not been deleted from the linked service, the role can't be deleted. This parameter includes a list of the resources that are associated with the role and the Region in which the resources are being used.
--
-- * 'dtfrtReason' - A short description of the reason that the service-linked role deletion failed.
deletionTaskFailureReasonType ::
  DeletionTaskFailureReasonType
deletionTaskFailureReasonType =
  DeletionTaskFailureReasonType'
    { _dtfrtRoleUsageList = Nothing,
      _dtfrtReason = Nothing
    }

-- | A list of objects that contains details about the service-linked role deletion failure, if that information is returned by the service. If the service-linked role has active sessions or if any resources that were used by the role have not been deleted from the linked service, the role can't be deleted. This parameter includes a list of the resources that are associated with the role and the Region in which the resources are being used.
dtfrtRoleUsageList :: Lens' DeletionTaskFailureReasonType [RoleUsageType]
dtfrtRoleUsageList = lens _dtfrtRoleUsageList (\s a -> s {_dtfrtRoleUsageList = a}) . _Default . _Coerce

-- | A short description of the reason that the service-linked role deletion failed.
dtfrtReason :: Lens' DeletionTaskFailureReasonType (Maybe Text)
dtfrtReason = lens _dtfrtReason (\s a -> s {_dtfrtReason = a})

instance FromXML DeletionTaskFailureReasonType where
  parseXML x =
    DeletionTaskFailureReasonType'
      <$> (x .@? "RoleUsageList" .!@ mempty >>= may (parseXMLList "member"))
      <*> (x .@? "Reason")

instance Hashable DeletionTaskFailureReasonType

instance NFData DeletionTaskFailureReasonType
