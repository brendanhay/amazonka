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
-- Module      : Amazonka.Shield.AssociateProactiveEngagementDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Initializes proactive engagement and sets the list of contacts for the
-- Shield Response Team (SRT) to use. You must provide at least one phone
-- number in the emergency contact list.
--
-- After you have initialized proactive engagement using this call, to
-- disable or enable proactive engagement, use the calls
-- @DisableProactiveEngagement@ and @EnableProactiveEngagement@.
--
-- This call defines the list of email addresses and phone numbers that the
-- SRT can use to contact you for escalations to the SRT and to initiate
-- proactive customer support.
--
-- The contacts that you provide in the request replace any contacts that
-- were already defined. If you already have contacts defined and want to
-- use them, retrieve the list using @DescribeEmergencyContactSettings@ and
-- then provide it to this call.
module Amazonka.Shield.AssociateProactiveEngagementDetails
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Shield.Types

-- | /See:/ 'newAssociateProactiveEngagementDetails' smart constructor.
data AssociateProactiveEngagementDetails = AssociateProactiveEngagementDetails'
  { -- | A list of email addresses and phone numbers that the Shield Response
    -- Team (SRT) can use to contact you for escalations to the SRT and to
    -- initiate proactive customer support.
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateProactiveEngagementDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'emergencyContactList', 'associateProactiveEngagementDetails_emergencyContactList' - A list of email addresses and phone numbers that the Shield Response
-- Team (SRT) can use to contact you for escalations to the SRT and to
-- initiate proactive customer support.
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

-- | A list of email addresses and phone numbers that the Shield Response
-- Team (SRT) can use to contact you for escalations to the SRT and to
-- initiate proactive customer support.
--
-- To enable proactive engagement, the contact list must include at least
-- one phone number.
--
-- The contacts that you provide here replace any contacts that were
-- already defined. If you already have contacts defined and want to use
-- them, retrieve the list using @DescribeEmergencyContactSettings@ and
-- then provide it here.
associateProactiveEngagementDetails_emergencyContactList :: Lens.Lens' AssociateProactiveEngagementDetails [EmergencyContact]
associateProactiveEngagementDetails_emergencyContactList = Lens.lens (\AssociateProactiveEngagementDetails' {emergencyContactList} -> emergencyContactList) (\s@AssociateProactiveEngagementDetails' {} a -> s {emergencyContactList = a} :: AssociateProactiveEngagementDetails) Prelude.. Lens.coerced

instance
  Core.AWSRequest
    AssociateProactiveEngagementDetails
  where
  type
    AWSResponse AssociateProactiveEngagementDetails =
      AssociateProactiveEngagementDetailsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          AssociateProactiveEngagementDetailsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    AssociateProactiveEngagementDetails
  where
  hashWithSalt
    _salt
    AssociateProactiveEngagementDetails' {..} =
      _salt `Prelude.hashWithSalt` emergencyContactList

instance
  Prelude.NFData
    AssociateProactiveEngagementDetails
  where
  rnf AssociateProactiveEngagementDetails' {..} =
    Prelude.rnf emergencyContactList

instance
  Data.ToHeaders
    AssociateProactiveEngagementDetails
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSShield_20160616.AssociateProactiveEngagementDetails" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    AssociateProactiveEngagementDetails
  where
  toJSON AssociateProactiveEngagementDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "EmergencyContactList"
                  Data..= emergencyContactList
              )
          ]
      )

instance
  Data.ToPath
    AssociateProactiveEngagementDetails
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    AssociateProactiveEngagementDetails
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAssociateProactiveEngagementDetailsResponse' smart constructor.
data AssociateProactiveEngagementDetailsResponse = AssociateProactiveEngagementDetailsResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  where
  rnf AssociateProactiveEngagementDetailsResponse' {..} =
    Prelude.rnf httpStatus
