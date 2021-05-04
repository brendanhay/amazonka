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
-- Module      : Network.AWS.SWF.UndeprecateActivityType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Undeprecates a previously deprecated /activity type/. After an activity
-- type has been undeprecated, you can create new tasks of that activity
-- type.
--
-- This operation is eventually consistent. The results are best effort and
-- may not exactly reflect recent updates and changes.
--
-- __Access Control__
--
-- You can use IAM policies to control this action\'s access to Amazon SWF
-- resources as follows:
--
-- -   Use a @Resource@ element with the domain name to limit the action to
--     only specified domains.
--
-- -   Use an @Action@ element to allow or deny permission to call this
--     action.
--
-- -   Constrain the following parameters by using a @Condition@ element
--     with the appropriate keys.
--
--     -   @activityType.name@: String constraint. The key is
--         @swf:activityType.name@.
--
--     -   @activityType.version@: String constraint. The key is
--         @swf:activityType.version@.
--
-- If the caller doesn\'t have sufficient permissions to invoke the action,
-- or the parameter values fall outside the specified constraints, the
-- action fails. The associated event attribute\'s @cause@ parameter is set
-- to @OPERATION_NOT_PERMITTED@. For details and example IAM policies, see
-- <https://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html Using IAM to Manage Access to Amazon SWF Workflows>
-- in the /Amazon SWF Developer Guide/.
module Network.AWS.SWF.UndeprecateActivityType
  ( -- * Creating a Request
    UndeprecateActivityType (..),
    newUndeprecateActivityType,

    -- * Request Lenses
    undeprecateActivityType_domain,
    undeprecateActivityType_activityType,

    -- * Destructuring the Response
    UndeprecateActivityTypeResponse (..),
    newUndeprecateActivityTypeResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SWF.Types

-- | /See:/ 'newUndeprecateActivityType' smart constructor.
data UndeprecateActivityType = UndeprecateActivityType'
  { -- | The name of the domain of the deprecated activity type.
    domain :: Prelude.Text,
    -- | The activity type to undeprecate.
    activityType :: ActivityType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UndeprecateActivityType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domain', 'undeprecateActivityType_domain' - The name of the domain of the deprecated activity type.
--
-- 'activityType', 'undeprecateActivityType_activityType' - The activity type to undeprecate.
newUndeprecateActivityType ::
  -- | 'domain'
  Prelude.Text ->
  -- | 'activityType'
  ActivityType ->
  UndeprecateActivityType
newUndeprecateActivityType pDomain_ pActivityType_ =
  UndeprecateActivityType'
    { domain = pDomain_,
      activityType = pActivityType_
    }

-- | The name of the domain of the deprecated activity type.
undeprecateActivityType_domain :: Lens.Lens' UndeprecateActivityType Prelude.Text
undeprecateActivityType_domain = Lens.lens (\UndeprecateActivityType' {domain} -> domain) (\s@UndeprecateActivityType' {} a -> s {domain = a} :: UndeprecateActivityType)

-- | The activity type to undeprecate.
undeprecateActivityType_activityType :: Lens.Lens' UndeprecateActivityType ActivityType
undeprecateActivityType_activityType = Lens.lens (\UndeprecateActivityType' {activityType} -> activityType) (\s@UndeprecateActivityType' {} a -> s {activityType = a} :: UndeprecateActivityType)

instance Prelude.AWSRequest UndeprecateActivityType where
  type
    Rs UndeprecateActivityType =
      UndeprecateActivityTypeResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull
      UndeprecateActivityTypeResponse'

instance Prelude.Hashable UndeprecateActivityType

instance Prelude.NFData UndeprecateActivityType

instance Prelude.ToHeaders UndeprecateActivityType where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "SimpleWorkflowService.UndeprecateActivityType" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.0" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON UndeprecateActivityType where
  toJSON UndeprecateActivityType' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("domain" Prelude..= domain),
            Prelude.Just
              ("activityType" Prelude..= activityType)
          ]
      )

instance Prelude.ToPath UndeprecateActivityType where
  toPath = Prelude.const "/"

instance Prelude.ToQuery UndeprecateActivityType where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUndeprecateActivityTypeResponse' smart constructor.
data UndeprecateActivityTypeResponse = UndeprecateActivityTypeResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UndeprecateActivityTypeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUndeprecateActivityTypeResponse ::
  UndeprecateActivityTypeResponse
newUndeprecateActivityTypeResponse =
  UndeprecateActivityTypeResponse'

instance
  Prelude.NFData
    UndeprecateActivityTypeResponse
