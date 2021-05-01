{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ModifyAvailabilityZoneGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes the opt-in status of the Local Zone and Wavelength Zone group
-- for your account.
--
-- Use
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DescribeAvailabilityZones.html DescribeAvailabilityZones>
-- to view the value for @GroupName@.
module Network.AWS.EC2.ModifyAvailabilityZoneGroup
  ( -- * Creating a Request
    ModifyAvailabilityZoneGroup (..),
    newModifyAvailabilityZoneGroup,

    -- * Request Lenses
    modifyAvailabilityZoneGroup_dryRun,
    modifyAvailabilityZoneGroup_groupName,
    modifyAvailabilityZoneGroup_optInStatus,

    -- * Destructuring the Response
    ModifyAvailabilityZoneGroupResponse (..),
    newModifyAvailabilityZoneGroupResponse,

    -- * Response Lenses
    modifyAvailabilityZoneGroupResponse_return,
    modifyAvailabilityZoneGroupResponse_httpStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newModifyAvailabilityZoneGroup' smart constructor.
data ModifyAvailabilityZoneGroup = ModifyAvailabilityZoneGroup'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The name of the Availability Zone group, Local Zone group, or Wavelength
    -- Zone group.
    groupName :: Prelude.Text,
    -- | Indicates whether you are opted in to the Local Zone group or Wavelength
    -- Zone group. The only valid value is @opted-in@. You must contact
    -- <https://console.aws.amazon.com/support/home#/case/create%3FissueType=customer-service%26serviceCode=general-info%26getting-started%26categoryCode=using-aws%26services AWS Support>
    -- to opt out of a Local Zone group, or Wavelength Zone group.
    optInStatus :: ModifyAvailabilityZoneOptInStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ModifyAvailabilityZoneGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'modifyAvailabilityZoneGroup_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'groupName', 'modifyAvailabilityZoneGroup_groupName' - The name of the Availability Zone group, Local Zone group, or Wavelength
-- Zone group.
--
-- 'optInStatus', 'modifyAvailabilityZoneGroup_optInStatus' - Indicates whether you are opted in to the Local Zone group or Wavelength
-- Zone group. The only valid value is @opted-in@. You must contact
-- <https://console.aws.amazon.com/support/home#/case/create%3FissueType=customer-service%26serviceCode=general-info%26getting-started%26categoryCode=using-aws%26services AWS Support>
-- to opt out of a Local Zone group, or Wavelength Zone group.
newModifyAvailabilityZoneGroup ::
  -- | 'groupName'
  Prelude.Text ->
  -- | 'optInStatus'
  ModifyAvailabilityZoneOptInStatus ->
  ModifyAvailabilityZoneGroup
newModifyAvailabilityZoneGroup
  pGroupName_
  pOptInStatus_ =
    ModifyAvailabilityZoneGroup'
      { dryRun =
          Prelude.Nothing,
        groupName = pGroupName_,
        optInStatus = pOptInStatus_
      }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
modifyAvailabilityZoneGroup_dryRun :: Lens.Lens' ModifyAvailabilityZoneGroup (Prelude.Maybe Prelude.Bool)
modifyAvailabilityZoneGroup_dryRun = Lens.lens (\ModifyAvailabilityZoneGroup' {dryRun} -> dryRun) (\s@ModifyAvailabilityZoneGroup' {} a -> s {dryRun = a} :: ModifyAvailabilityZoneGroup)

-- | The name of the Availability Zone group, Local Zone group, or Wavelength
-- Zone group.
modifyAvailabilityZoneGroup_groupName :: Lens.Lens' ModifyAvailabilityZoneGroup Prelude.Text
modifyAvailabilityZoneGroup_groupName = Lens.lens (\ModifyAvailabilityZoneGroup' {groupName} -> groupName) (\s@ModifyAvailabilityZoneGroup' {} a -> s {groupName = a} :: ModifyAvailabilityZoneGroup)

-- | Indicates whether you are opted in to the Local Zone group or Wavelength
-- Zone group. The only valid value is @opted-in@. You must contact
-- <https://console.aws.amazon.com/support/home#/case/create%3FissueType=customer-service%26serviceCode=general-info%26getting-started%26categoryCode=using-aws%26services AWS Support>
-- to opt out of a Local Zone group, or Wavelength Zone group.
modifyAvailabilityZoneGroup_optInStatus :: Lens.Lens' ModifyAvailabilityZoneGroup ModifyAvailabilityZoneOptInStatus
modifyAvailabilityZoneGroup_optInStatus = Lens.lens (\ModifyAvailabilityZoneGroup' {optInStatus} -> optInStatus) (\s@ModifyAvailabilityZoneGroup' {} a -> s {optInStatus = a} :: ModifyAvailabilityZoneGroup)

instance
  Prelude.AWSRequest
    ModifyAvailabilityZoneGroup
  where
  type
    Rs ModifyAvailabilityZoneGroup =
      ModifyAvailabilityZoneGroupResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          ModifyAvailabilityZoneGroupResponse'
            Prelude.<$> (x Prelude..@? "return")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ModifyAvailabilityZoneGroup

instance Prelude.NFData ModifyAvailabilityZoneGroup

instance
  Prelude.ToHeaders
    ModifyAvailabilityZoneGroup
  where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath ModifyAvailabilityZoneGroup where
  toPath = Prelude.const "/"

instance Prelude.ToQuery ModifyAvailabilityZoneGroup where
  toQuery ModifyAvailabilityZoneGroup' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ( "ModifyAvailabilityZoneGroup" ::
                         Prelude.ByteString
                     ),
        "Version"
          Prelude.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Prelude.=: dryRun,
        "GroupName" Prelude.=: groupName,
        "OptInStatus" Prelude.=: optInStatus
      ]

-- | /See:/ 'newModifyAvailabilityZoneGroupResponse' smart constructor.
data ModifyAvailabilityZoneGroupResponse = ModifyAvailabilityZoneGroupResponse'
  { -- | Is @true@ if the request succeeds, and an error otherwise.
    return' :: Prelude.Maybe Prelude.Bool,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ModifyAvailabilityZoneGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'return'', 'modifyAvailabilityZoneGroupResponse_return' - Is @true@ if the request succeeds, and an error otherwise.
--
-- 'httpStatus', 'modifyAvailabilityZoneGroupResponse_httpStatus' - The response's http status code.
newModifyAvailabilityZoneGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ModifyAvailabilityZoneGroupResponse
newModifyAvailabilityZoneGroupResponse pHttpStatus_ =
  ModifyAvailabilityZoneGroupResponse'
    { return' =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Is @true@ if the request succeeds, and an error otherwise.
modifyAvailabilityZoneGroupResponse_return :: Lens.Lens' ModifyAvailabilityZoneGroupResponse (Prelude.Maybe Prelude.Bool)
modifyAvailabilityZoneGroupResponse_return = Lens.lens (\ModifyAvailabilityZoneGroupResponse' {return'} -> return') (\s@ModifyAvailabilityZoneGroupResponse' {} a -> s {return' = a} :: ModifyAvailabilityZoneGroupResponse)

-- | The response's http status code.
modifyAvailabilityZoneGroupResponse_httpStatus :: Lens.Lens' ModifyAvailabilityZoneGroupResponse Prelude.Int
modifyAvailabilityZoneGroupResponse_httpStatus = Lens.lens (\ModifyAvailabilityZoneGroupResponse' {httpStatus} -> httpStatus) (\s@ModifyAvailabilityZoneGroupResponse' {} a -> s {httpStatus = a} :: ModifyAvailabilityZoneGroupResponse)

instance
  Prelude.NFData
    ModifyAvailabilityZoneGroupResponse
