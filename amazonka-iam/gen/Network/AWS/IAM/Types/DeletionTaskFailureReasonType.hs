{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.DeletionTaskFailureReasonType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.DeletionTaskFailureReasonType where

import Network.AWS.IAM.Types.RoleUsageType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The reason that the service-linked role deletion failed.
--
-- This data type is used as a response element in the
-- GetServiceLinkedRoleDeletionStatus operation.
--
-- /See:/ 'newDeletionTaskFailureReasonType' smart constructor.
data DeletionTaskFailureReasonType = DeletionTaskFailureReasonType'
  { -- | A short description of the reason that the service-linked role deletion
    -- failed.
    reason :: Prelude.Maybe Prelude.Text,
    -- | A list of objects that contains details about the service-linked role
    -- deletion failure, if that information is returned by the service. If the
    -- service-linked role has active sessions or if any resources that were
    -- used by the role have not been deleted from the linked service, the role
    -- can\'t be deleted. This parameter includes a list of the resources that
    -- are associated with the role and the Region in which the resources are
    -- being used.
    roleUsageList :: Prelude.Maybe [RoleUsageType]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeletionTaskFailureReasonType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'reason', 'deletionTaskFailureReasonType_reason' - A short description of the reason that the service-linked role deletion
-- failed.
--
-- 'roleUsageList', 'deletionTaskFailureReasonType_roleUsageList' - A list of objects that contains details about the service-linked role
-- deletion failure, if that information is returned by the service. If the
-- service-linked role has active sessions or if any resources that were
-- used by the role have not been deleted from the linked service, the role
-- can\'t be deleted. This parameter includes a list of the resources that
-- are associated with the role and the Region in which the resources are
-- being used.
newDeletionTaskFailureReasonType ::
  DeletionTaskFailureReasonType
newDeletionTaskFailureReasonType =
  DeletionTaskFailureReasonType'
    { reason =
        Prelude.Nothing,
      roleUsageList = Prelude.Nothing
    }

-- | A short description of the reason that the service-linked role deletion
-- failed.
deletionTaskFailureReasonType_reason :: Lens.Lens' DeletionTaskFailureReasonType (Prelude.Maybe Prelude.Text)
deletionTaskFailureReasonType_reason = Lens.lens (\DeletionTaskFailureReasonType' {reason} -> reason) (\s@DeletionTaskFailureReasonType' {} a -> s {reason = a} :: DeletionTaskFailureReasonType)

-- | A list of objects that contains details about the service-linked role
-- deletion failure, if that information is returned by the service. If the
-- service-linked role has active sessions or if any resources that were
-- used by the role have not been deleted from the linked service, the role
-- can\'t be deleted. This parameter includes a list of the resources that
-- are associated with the role and the Region in which the resources are
-- being used.
deletionTaskFailureReasonType_roleUsageList :: Lens.Lens' DeletionTaskFailureReasonType (Prelude.Maybe [RoleUsageType])
deletionTaskFailureReasonType_roleUsageList = Lens.lens (\DeletionTaskFailureReasonType' {roleUsageList} -> roleUsageList) (\s@DeletionTaskFailureReasonType' {} a -> s {roleUsageList = a} :: DeletionTaskFailureReasonType) Prelude.. Lens.mapping Prelude._Coerce

instance
  Prelude.FromXML
    DeletionTaskFailureReasonType
  where
  parseXML x =
    DeletionTaskFailureReasonType'
      Prelude.<$> (x Prelude..@? "Reason")
      Prelude.<*> ( x Prelude..@? "RoleUsageList"
                      Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "member")
                  )

instance
  Prelude.Hashable
    DeletionTaskFailureReasonType

instance Prelude.NFData DeletionTaskFailureReasonType
