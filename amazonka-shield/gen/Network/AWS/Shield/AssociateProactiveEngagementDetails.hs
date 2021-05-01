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
-- Module      : Network.AWS.Shield.AssociateProactiveEngagementDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Initializes proactive engagement and sets the list of contacts for the
-- DDoS Response Team (DRT) to use. You must provide at least one phone
-- number in the emergency contact list.
--
-- After you have initialized proactive engagement using this call, to
-- disable or enable proactive engagement, use the calls
-- @DisableProactiveEngagement@ and @EnableProactiveEngagement@.
--
-- This call defines the list of email addresses and phone numbers that the
-- DDoS Response Team (DRT) can use to contact you for escalations to the
-- DRT and to initiate proactive customer support.
--
-- The contacts that you provide in the request replace any contacts that
-- were already defined. If you already have contacts defined and want to
-- use them, retrieve the list using @DescribeEmergencyContactSettings@ and
-- then provide it to this call.
module Network.AWS.Shield.AssociateProactiveEngagementDetails
  ( -- * Creating a Request
    AssociateProactiveEngagementDetails (..),
    newAssociateProactiveEngagementDetails,

    -- * Request Lenses
    associateProactiveEngagementDetails_emergencyContactList,

    -- * Destructuring the Response
    AssociateProactiveEngagementDetailsResponse (..),
    newAssociateProactiveEngagementDetailsResponse,

    -- * Response Lenses
    associateProactiveEngagementDetailsResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Shield.Types

-- | /See:/ 'newAssociateProactiveEngagementDetails' smart constructor.
data AssociateProactiveEngagementDetails = AssociateProactiveEngagementDetails'
  { -- | A list of email addresses and phone numbers that the DDoS Response Team
    -- (DRT) can use to contact you for escalations to the DRT and to initiate
    -- proactive customer support.
    --
    -- To enable proactive engagement, the contact list must include at least
    -- one phone number.
    --
    -- The contacts that you provide here replace any contacts that were
    -- already defined. If you already have contacts defined and want to use
    -- them, retrieve the list using @DescribeEmergencyContactSettings@ and
    -- then provide it here.
    emergencyContactList :: [EmergencyContact]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AssociateProactiveEngagementDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'emergencyContactList', 'associateProactiveEngagementDetails_emergencyContactList' - A list of email addresses and phone numbers that the DDoS Response Team
-- (DRT) can use to contact you for escalations to the DRT and to initiate
-- proactive customer support.
--
-- To enable proactive engagement, the contact list must include at least
-- one phone number.
--
-- The contacts that you provide here replace any contacts that were
-- already defined. If you already have contacts defined and want to use
-- them, retrieve the list using @DescribeEmergencyContactSettings@ and
-- then provide it here.
newAssociateProactiveEngagementDetails ::
  AssociateProactiveEngagementDetails
newAssociateProactiveEngagementDetails =
  AssociateProactiveEngagementDetails'
    { emergencyContactList =
        Prelude.mempty
    }

-- | A list of email addresses and phone numbers that the DDoS Response Team
-- (DRT) can use to contact you for escalations to the DRT and to initiate
-- proactive customer support.
--
-- To enable proactive engagement, the contact list must include at least
-- one phone number.
--
-- The contacts that you provide here replace any contacts that were
-- already defined. If you already have contacts defined and want to use
-- them, retrieve the list using @DescribeEmergencyContactSettings@ and
-- then provide it here.
associateProactiveEngagementDetails_emergencyContactList :: Lens.Lens' AssociateProactiveEngagementDetails [EmergencyContact]
associateProactiveEngagementDetails_emergencyContactList = Lens.lens (\AssociateProactiveEngagementDetails' {emergencyContactList} -> emergencyContactList) (\s@AssociateProactiveEngagementDetails' {} a -> s {emergencyContactList = a} :: AssociateProactiveEngagementDetails) Prelude.. Prelude._Coerce

instance
  Prelude.AWSRequest
    AssociateProactiveEngagementDetails
  where
  type
    Rs AssociateProactiveEngagementDetails =
      AssociateProactiveEngagementDetailsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          AssociateProactiveEngagementDetailsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    AssociateProactiveEngagementDetails

instance
  Prelude.NFData
    AssociateProactiveEngagementDetails

instance
  Prelude.ToHeaders
    AssociateProactiveEngagementDetails
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSShield_20160616.AssociateProactiveEngagementDetails" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance
  Prelude.ToJSON
    AssociateProactiveEngagementDetails
  where
  toJSON AssociateProactiveEngagementDetails' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "EmergencyContactList"
                  Prelude..= emergencyContactList
              )
          ]
      )

instance
  Prelude.ToPath
    AssociateProactiveEngagementDetails
  where
  toPath = Prelude.const "/"

instance
  Prelude.ToQuery
    AssociateProactiveEngagementDetails
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAssociateProactiveEngagementDetailsResponse' smart constructor.
data AssociateProactiveEngagementDetailsResponse = AssociateProactiveEngagementDetailsResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AssociateProactiveEngagementDetailsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'associateProactiveEngagementDetailsResponse_httpStatus' - The response's http status code.
newAssociateProactiveEngagementDetailsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AssociateProactiveEngagementDetailsResponse
newAssociateProactiveEngagementDetailsResponse
  pHttpStatus_ =
    AssociateProactiveEngagementDetailsResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
associateProactiveEngagementDetailsResponse_httpStatus :: Lens.Lens' AssociateProactiveEngagementDetailsResponse Prelude.Int
associateProactiveEngagementDetailsResponse_httpStatus = Lens.lens (\AssociateProactiveEngagementDetailsResponse' {httpStatus} -> httpStatus) (\s@AssociateProactiveEngagementDetailsResponse' {} a -> s {httpStatus = a} :: AssociateProactiveEngagementDetailsResponse)

instance
  Prelude.NFData
    AssociateProactiveEngagementDetailsResponse
